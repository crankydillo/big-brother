@ECHO OFF

set APP_HOME=%~dp0..

java -Dscallop.app.config=%APP_HOME%/conf/app.properties -Dlog4j.configuration=file:/%APP_HOME%/conf/log4j.properties -cp "%APP_HOME%/lib/*;" org.beeherd.metrics.BigBrotherApp %*

set APP_HOME=
