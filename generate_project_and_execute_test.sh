# Script for jenkins to test the template generation and verifying the generated project by running all the tests against it.
echo "Generating 'api-template-test' project"
echo "Remove 'api-template-test' project if exists"


#create project 'api-template-test' from workspace root by default ( '.')
cd $WORKSPACE
python bin/create.py api-template-test .

echo "Execute tests on generated 'api-template-test'"
cd $WORKSPACE/api-template-test
sbt clean compile test it:test component:test

echo "copying reports"
cp -R $WORKSPACE/api-template-test/target $WORKSPACE/target