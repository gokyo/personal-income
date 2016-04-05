# Script for local run to test the template generation and verifying the generated project by running all the tests against it.
echo "Generating 'api-template-test' project"

TEST_PROJECT_NAME=api-template-test
TEST_TEMPLATE_PATH=$WORKSPACE/$TEST_PROJECT_NAME

read -p "Are you ok with deleting '$TEST_TEMPLATE_PATH' if exists (y/n)? " -n 1 -r
echo    # (optional) move to a new line
if [[ $REPLY =~ ^[Yy]$ ]]
then
    rm -rf $TEST_TEMPLATE_PATH
    python bin/create.py $TEST_PROJECT_NAME

    echo "Execute tests on generated 'api-template-test'"
    cd $TEST_TEMPLATE_PATH
    sbt clean compile test it:test component:test
    echo "Clean up, remove '$TEST_TEMPLATE_PATH'"
    rm -rf $TEST_TEMPLATE_PATH
else
    echo "Generating api-template-test interrupted"
fi
