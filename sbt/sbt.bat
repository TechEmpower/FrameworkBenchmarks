set SCRIPT_DIR=%~dp0
java -Xss2M -XX:MaxPermSize=512M -Xmx1536M -XX:+CMSClassUnloadingEnabled -jar "%SCRIPT_DIR%sbt-launch.jar" %*
