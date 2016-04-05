
set -e

sbt test
sbt it:test
sbt component:test
