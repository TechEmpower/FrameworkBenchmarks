rem -Xms64m -Xmx2048m

@echo off
setlocal & pushd
set APP_ENTRY=org.tio.http.server.benchmark.TioBenchmarkStarter
set BASE=%~dp0
set CP=%BASE%\config;%BASE%\lib\*
java -server -Xverify:none -Xms1G -Xmx1G -Dpacket.handler.mode=queue -cp "%CP%" %APP_ENTRY%
endlocal & popd



