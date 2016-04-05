#!/usr/bin/env python

import sys
import argparse
import os
import distutils.core
import string 
import random
import subprocess
import fileinput
import shutil
import pyratemp
import urllib2
import base64
from xml.dom.minidom import parse

def required_environment_directory(environment_variable, description_for_error):
    directory = os.environ.get(environment_variable, None)
    if not directory:
        print "'%s' environment variable is required. You can add this to your ~/.bash_profile by adding the line %s=[%s]" % (environment_variable, environment_variable, description_for_error)
        exit(1)    
    directory = os.path.abspath(directory)
    if not os.path.isdir(directory):
        print "Error: '%s' environment variable points to non-existent directory: %s" % (environment_variable, directory)
        sys.exit(1)
    return directory

workspace = required_environment_directory("WORKSPACE", "your workspace root dir")

def get_latest_version(artifact, scalaVersion="_2.11"):
    artifactWithVersion = artifact + scalaVersion
    local_nexus_version = find_version_in(get_version_info_from_nexus_dev(artifactWithVersion))
    sonatype_version    = find_version_in(get_version_info_from_sonatype(artifactWithVersion))
    return max_version_of(local_nexus_version, sonatype_version)

def get_latest_version_in_open(artifact, scalaVersion="_2.11"):
    artifactWithVersion = artifact + scalaVersion
    maven_metadata = get_version_info_from_bintray(artifactWithVersion)

    try:
        data = maven_metadata.getElementsByTagName("versioning")[0]
    except:
        self.context.log("Unable to get latest version from bintray")
        return None

    latestVersion =  data.getElementsByTagName("latest")[0].firstChild.nodeValue
    return latestVersion

def max_version_of(*args):
    def rank(ver):
        ver = ver or ""
        return [int(s) for s in ver.split(".") if s.isdigit()]
    return sorted(args, key=rank, reverse=True)[0]


def find_version_in(dom):
    latest = "latestRelease"
    try:
        data = dom.getElementsByTagName("artifact")[0]
        latestNode = data.getElementsByTagName(latest)[0]
    except:
        return None
    return latestNode.firstChild.nodeValue


def get_version_info_from_sonatype(artifact):
    sonatype_nexus = "https://oss.sonatype.org/service/local/lucene/search?a=" + artifact
    request = urllib2.Request(sonatype_nexus)
    response = urllib2.urlopen(request)
    dom = parse(response)
    response.close()
    return dom

def get_version_info_from_nexus_dev(artifact):
    lucene_nexus = "https://nexus-dev.tax.service.gov.uk/service/local/lucene/search?a=" + artifact
    request = urllib2.Request(lucene_nexus)
    base64string = base64.encodestring(_header_credentials()).replace('\n', '')
    request.add_header("Authorization", "Basic %s" % base64string)
    response = urllib2.urlopen(request)
    dom = parse(response)
    response.close()
    return dom

def get_version_info_from_bintray(artifact):
    bintray = "https://dl.bintray.com/hmrc/releases/uk/gov/hmrc/" + artifact + "/maven-metadata.xml"
    request = urllib2.Request(bintray)
    response = urllib2.urlopen(request)
    dom = parse(response)
    response.close()
    return dom


def lookup_credentials():
    sbt_credentials = os.environ["HOME"] + "/.sbt/.credentials"
    if not os.path.exists(sbt_credentials):
        print "Cannot look up nexus credentials from " + sbt_credentials
        return {}
    return {key.strip(): value.strip() for (key, value) in map(lambda x: x.split("=", 1), open(sbt_credentials, 'r').readlines())}

def _header_credentials():
    credentials = lookup_credentials()
    return credentials["user"] + ":" + credentials["password"]

def generate_app_secret():
    lst = [random.choice(string.ascii_letters + string.digits) for n in xrange(64)]
    application_secret = ''.join(lst)
    return application_secret


def replace_variables_for_app(folder_to_search, application_name, service_type):
    govukTemplateVersion=get_latest_version_in_open("govuk-template")
    frontendBootstrapVersion=get_latest_version_in_open("frontend-bootstrap")
    playUiVersion=get_latest_version_in_open("play-ui")
    playPartialsVersion=get_latest_version_in_open("play-partials")
    playAuthVersion=get_latest_version_in_open("play-authorisation")
    playAuthorisedFrontendVersion=get_latest_version("play-authorised-frontend")
    microserviceBootstrapVersion=get_latest_version_in_open("microservice-bootstrap")
    playUrlBindersVersion=get_latest_version_in_open("play-url-binders")
    playConfigVersion=get_latest_version_in_open("play-config")
    domainVersion=get_latest_version_in_open("domain")
    hmrcTestVersion=get_latest_version_in_open("hmrctest")
    stubsCoreVersion=get_latest_version("hmrc-stubs-core")
    playHealthVersion=get_latest_version_in_open("play-health")
    assetsFrontendVersion=get_latest_version("assets-frontend", "")
    for subdir, dirs, files in os.walk(folder_to_search):
        for f in files:
            if f in ["create.py", "pyratemp.py", "pyratemp.pyc"]:
                continue
            file_name = os.path.join(subdir, f)

            # exlude target dir and hidden folders
            if file_name.find("target/") != -1 or file_name.find('/.') != -1:
                continue

            t = pyratemp.Template(filename=file_name)
            file_content = t(UPPER_CASE_APP_NAME=application_name.upper(),
                             UPPER_CASE_APP_NAME_UNDERSCORE_ONLY=application_name.upper().replace("-", "_"),
                             APP_NAME=application_name,
                             SECRET_KEY=generate_app_secret(),
                             type=service_type,
                             govukTemplateVersion=govukTemplateVersion,
                             microserviceBootstrapVersion=microserviceBootstrapVersion,
                             playUrlBindersVersion=playUrlBindersVersion,
                             playConfigVersion=playConfigVersion,
                             domainVersion=domainVersion,
                             hmrcTestVersion=hmrcTestVersion,
                             frontendBootstrapVersion=frontendBootstrapVersion,
                             playUiVersion=playUiVersion,
                             playAuthVersion=playAuthVersion,
                             playPartialsVersion=playPartialsVersion,
                             playAuthorisedFrontendVersion=playAuthorisedFrontendVersion,
                             stubsCoreVersion=stubsCoreVersion,
                             playHealthVersion=playHealthVersion,
                             assetsFrontendVersion=assetsFrontendVersion,
                             bashbang="#!/bin/bash",
                             shbang="#!/bin/sh",
                             )
            write_to_file(file_name, file_content)


def write_to_file(f, file_content):
    open_file = open(f, 'w')
    open_file.write(file_content)
    open_file.close()


def replace_in_file(file_to_search, replace_this, with_this):
    for line in fileinput.input(file_to_search, inplace=True):
        print line.replace(replace_this, with_this),


def delete_bin_files(project_folder):
    file_name = os.path.join(project_folder, "bin")
    if os.path.isdir(file_name):
        shutil.rmtree(file_name)


def call(command, quiet=True):
    if not quiet:
        print "calling: '" + command + "' from: '" + os.getcwd() + "'"
    ps_command = subprocess.Popen(command, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    ps_command.wait()
    return ps_command


def initialise_git_repo(organisation, folder, project_name):
    os.chdir(folder)
    call('git init .')
    call('git add . -A')
    call('git commit -m \"Creating new service %s\"' % project_name)
    call('git remote add origin git@github.tools.tax.service.gov.uk:%s/%s.git' % (organisation, project_name))

def create_service(project_root_name, service_type, organisation, templateFolderName):
    project_name = project_root_name
    print "Creating new microservice: %s, this could take a few moments" % project_name
    project_folder = os.path.normpath(os.path.join(workspace, project_name))
    if os.path.isdir(project_folder):
        print "The folder '%s' already exists, not creating microservice " % str(project_folder)
    else:
        stub_template_folder = os.path.join(workspace, templateFolderName)
        print "Copy project from template folder '%s' to project folder '%s'" % (stub_template_folder, project_folder)
        distutils.dir_util.copy_tree(stub_template_folder, project_folder)
        shutil.rmtree(os.path.join(project_folder, ".git"))
        replace_variables_for_app(project_folder, project_name, service_type)
        initialise_git_repo(organisation, project_folder, project_name) 
        print "Created microservice at '%s'. You can now finish by doing the following from the new dir" % project_folder
        print "git push -u origin master"
        print "----"

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Template Creation Tool - Create an new project... fast!')
    parser.add_argument('PROJECT_NAME', type=str, help='The name of the project you want to create')
    parser.add_argument('TEMPLATE_FOLDER_NAME', type=str, nargs='?', default='api-microservice-template', help='The name of the template folder')
    args = parser.parse_args()

    organisation = 'HMRC'
    create_service(args.PROJECT_NAME, "MICROSERVICE", organisation, args.TEMPLATE_FOLDER_NAME)
