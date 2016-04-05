api-microservice-template
=============

### Info
Use this project to create a Microservice that needs to register itself to the API Platform.

This project is based on [HMRC Play Template](https://github.tools.tax.service.gov.uk/HMRC/hmrc-play-template)


###Prerequsites

#### SBT Credentials

Create ~/.sbt/.credentials file with the following information:
```
realm=Sonatype Nexus Repository Manager
host=nexus-dev.tax.service.gov.uk
user=<nexus-username>
password=<nexus-password>
```

note: That you will need to ask the Ops team for a Nexus username and password


#### Required environmental variables
WORKSPACE=<root-path>/hmrc-development-environment/hmrc/

#### Install
Python

### How to generate new microservice
- Checkout ```api-microservice-template``` project in workspace
- Open terminal, go to ```api-microservice-template/bin``` directory
- Be sure that your ```$WORKSPACE``` environment variable is set to the directory you want to generate the project to
- Be sure that the directory your ```$WORKSPACE``` is set to contains this project in the folder ```api-microservice-template```
- Run ```python create.py <name of your microservice>``` command

<blockquote>
After a successful execution of the create.py 
script your $WORKSPACE folder should contain the generated microservice in the given '<name of your microservice>' folder with the latest dependencies used and a repository initialized.
</blockquote>

### Functionalites provided out of the box :
- Example Service For Sandbox, returning canned data
- Example Service For Live, calling connector to a DES
- Registration in Service Locator on start-up
- Standardized error handling
- Auditing of the example request
- Authentication on live response
- Accept header validation
- Definition.json for the module
- Documentation XML for the example endpoint

### Basic Architecture
The service is built with a basic controller-service-connector architecture.

The **Controller** layer provides 2 endpoints, one sandbox and one live. 
The Sandbox endpoint is used for testing, it provides the same signature as the live, but it calls a Sandbox Service. 

The **Service** layer has a Live and a Sandbox service too; The Sandbox Service returns the predefined response while the Live Service calls the Connector to fetch the response from the DES Backend. 

The **Connector** layer handles all communication with the DES Backend. 

### Infrastructure
The Service is to be deployed to the following infrasructure: ***REMOVED***

On start up the Microservice registers itself to the service locator and expose endpoints to it to return all the definitions of the services (api/definition.json) and documentations for each provided services.
Once the service is registered on start up the Platform is able to provide access to 3rd pty to call the services with registering to WSO2 gateway. The endpoint status determines whether the LIVE endpoint will be accessible (PUBLISHED) or only the Sandbox endpoint can be reached (PROTOTYPE).

### How to Start
You need to add remote HEAD for sbt to be able to build, by executing the following steps
- Create a github repository for your microservice
- Clone this project and rename it according to your microservice.
- Change readme, add and commit it. 
- Push to remote. 

### Run Example Microservice 
At this stage you should be able to compile & run the Microservice with executing ```./run_in_stub_mode.sh```

Once the service is up, you can hit the endpoint: http://localhost:9000/sandbox/sa/1097172564/example
with sending a valid Header **Accept=application/vnd.hmrc.1.0+json** and get the following Canned Response:
```javascript
{"text":"example","number":1.00}
```

### Configure the template to your project
| File          | Changes           | Notes |
| ------------- | ------------- |------------- | 
| domain.Example.scala                         | - rename it to your reflect your microservice domain | |
| connectors.ExampleBackendConnector.scala     | - rename it to your reflect your microservice domain | |
|                                              | - change DES service URL | |
| controllers.ExampleController.scala          | - rename it to your reflect your microservice domain      |  |
| services.ExampleService.scala                | - rename it to your reflect your microservice domain | |
| conf/application.conf                        | - change appName, appUrl | |
|                                              | - rename controller configuration, to the name you renamed the controller to |  |
| conf/live.routes                        | - change controller and method name| |
| conf/sandbox.routes                        | - change controller and method name| |
| feature/*.feature                        | - change feature files according to your service needs| |
| public/documentation                        | - change documentation to reflect your service| The api.versions[].endpoints[].endpointName should match the documentation file name with white spaces replaced by '-' character |
|||


### Testing
THe generated project will contain ```./run_all_tests.sh``` to run all the tests provided with the project, including unit, integration and component tests. 

#### Unit tests
Located in the [test/unit](https://github.tools.tax.service.gov.uk/HMRC/api-microservice-template/tree/master/test/it) folder, testing functionalities including formatting and validation of certain types and required headers.

Run ```sbt test``` to execute them.

#### Integration tests
Located in the  [test/it](https://github.tools.tax.service.gov.uk/HMRC/api-microservice-template/tree/master/test/it) folder, testing the platform integration.

The [PlatformIntegrationSpec.scala](https://github.tools.tax.service.gov.uk/HMRC/api-microservice-template/blob/master/test/it/PlatformIntegrationSpec.scala) will check whether the service registers itself to service locator and whether it provides all the neccessary endpoints required by the platform for the documentation to be generated. 

Run ```sbt it:test``` to execute them.

#### Component tests
Located in the [test/component](https://github.tools.tax.service.gov.uk/HMRC/api-microservice-template/tree/master/test/component) folder, testing the component as a whole isolated from any backend with the help of mockservers.

The test steps are defined as [features](https://github.tools.tax.service.gov.uk/HMRC/api-microservice-template/tree/master/features) and cucumber is used to run them. 

Run ```sbt component:test``` to execute them.

### Changing this template
To change this template you need to make sure that any change you made is working in the generated project. 

For this you need to run [generate_project_and_execute_test.sh](https://github.tools.tax.service.gov.uk/HMRC/api-microservice-template/blob/master/generate_project_and_execute_test.sh) that generates a project ```api-template-test``` and runs test against it. 
