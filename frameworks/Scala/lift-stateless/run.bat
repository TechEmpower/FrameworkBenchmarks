cd %~dp0\target
java -server -XX:+CMSClassUnloadingEnabled -Xmx2G -Xss512M -XX:MaxPermSize=512m -jar lift-stateless-assembly-0.0.1.jar > nul 2>&1
cd %~dp0
