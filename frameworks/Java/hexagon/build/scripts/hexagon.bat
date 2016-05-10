@if "%DEBUG%" == "" @echo off
@rem ##########################################################################
@rem
@rem  hexagon startup script for Windows
@rem
@rem ##########################################################################

@rem Set local scope for the variables with windows NT shell
if "%OS%"=="Windows_NT" setlocal

set DIRNAME=%~dp0
if "%DIRNAME%" == "" set DIRNAME=.
set APP_BASE_NAME=%~n0
set APP_HOME=%DIRNAME%..

@rem Add default JVM options here. You can also use JAVA_OPTS and HEXAGON_OPTS to pass JVM options to this script.
set DEFAULT_JVM_OPTS="-Xms128M" "-Xmx128M" "-server" "-XX:+UseNUMA" "-XX:+UseParallelGC" "-XX:+AggressiveOpts"

@rem Find java.exe
if defined JAVA_HOME goto findJavaFromJavaHome

set JAVA_EXE=java.exe
%JAVA_EXE% -version >NUL 2>&1
if "%ERRORLEVEL%" == "0" goto init

echo.
echo ERROR: JAVA_HOME is not set and no 'java' command could be found in your PATH.
echo.
echo Please set the JAVA_HOME variable in your environment to match the
echo location of your Java installation.

goto fail

:findJavaFromJavaHome
set JAVA_HOME=%JAVA_HOME:"=%
set JAVA_EXE=%JAVA_HOME%/bin/java.exe

if exist "%JAVA_EXE%" goto init

echo.
echo ERROR: JAVA_HOME is set to an invalid directory: %JAVA_HOME%
echo.
echo Please set the JAVA_HOME variable in your environment to match the
echo location of your Java installation.

goto fail

:init
@rem Get command-line arguments, handling Windows variants

if not "%OS%" == "Windows_NT" goto win9xME_args
if "%@eval[2+2]" == "4" goto 4NT_args

:win9xME_args
@rem Slurp the command line arguments.
set CMD_LINE_ARGS=
set _SKIP=2

:win9xME_args_slurp
if "x%~1" == "x" goto execute

set CMD_LINE_ARGS=%*
goto execute

:4NT_args
@rem Get arguments from the 4NT Shell from JP Software
set CMD_LINE_ARGS=%$

:execute
@rem Setup the command line

set CLASSPATH=%APP_HOME%\lib\hexagon-1.0.0.jar;%APP_HOME%\lib\hexagon-0.1.5.jar;%APP_HOME%\lib\kotlin-reflect-1.0.1-2.jar;%APP_HOME%\lib\amqp-client-3.6.1.jar;%APP_HOME%\lib\mongo-java-driver-3.2.2.jar;%APP_HOME%\lib\logback-classic-1.1.7.jar;%APP_HOME%\lib\okhttp-3.2.0.jar;%APP_HOME%\lib\cron-utils-3.1.6.jar;%APP_HOME%\lib\jackson-datatype-jdk8-2.7.4.jar;%APP_HOME%\lib\jackson-datatype-jsr310-2.7.4.jar;%APP_HOME%\lib\jackson-dataformat-xml-2.7.4.jar;%APP_HOME%\lib\jackson-dataformat-yaml-2.7.4.jar;%APP_HOME%\lib\jackson-module-kotlin-2.7.4.jar;%APP_HOME%\lib\ratpack-core-1.3.3.jar;%APP_HOME%\lib\pebble-2.2.1.jar;%APP_HOME%\lib\kotlin-stdlib-1.0.1-2.jar;%APP_HOME%\lib\logback-core-1.1.7.jar;%APP_HOME%\lib\slf4j-api-1.7.20.jar;%APP_HOME%\lib\okio-1.6.0.jar;%APP_HOME%\lib\commons-lang3-3.4.jar;%APP_HOME%\lib\joda-time-2.8.2.jar;%APP_HOME%\lib\guava-18.0.jar;%APP_HOME%\lib\htime-1.0.0.jar;%APP_HOME%\lib\jackson-core-2.7.4.jar;%APP_HOME%\lib\jackson-databind-2.7.4.jar;%APP_HOME%\lib\jackson-module-jaxb-annotations-2.7.4.jar;%APP_HOME%\lib\stax2-api-3.1.4.jar;%APP_HOME%\lib\snakeyaml-1.15.jar;%APP_HOME%\lib\jackson-datatype-jdk7-2.6.2.jar;%APP_HOME%\lib\netty-codec-http-4.1.0.CR7.jar;%APP_HOME%\lib\netty-handler-4.1.0.CR7.jar;%APP_HOME%\lib\netty-transport-native-epoll-4.1.0.CR7-linux-x86_64.jar;%APP_HOME%\lib\caffeine-2.2.6.jar;%APP_HOME%\lib\javassist-3.19.0-GA.jar;%APP_HOME%\lib\jackson-datatype-guava-2.6.2.jar;%APP_HOME%\lib\reactive-streams-1.0.0.jar;%APP_HOME%\lib\coverity-escapers-1.1.jar;%APP_HOME%\lib\kotlin-runtime-1.0.1-2.jar;%APP_HOME%\lib\netty-codec-4.1.0.CR7.jar;%APP_HOME%\lib\netty-buffer-4.1.0.CR7.jar;%APP_HOME%\lib\netty-transport-4.1.0.CR7.jar;%APP_HOME%\lib\netty-common-4.1.0.CR7.jar;%APP_HOME%\lib\netty-resolver-4.1.0.CR7.jar;%APP_HOME%\lib\jackson-annotations-2.7.4.jar

@rem Execute hexagon
"%JAVA_EXE%" %DEFAULT_JVM_OPTS% %JAVA_OPTS% %HEXAGON_OPTS%  -classpath "%CLASSPATH%" co.there4.hexagon.BenchmarkKt %CMD_LINE_ARGS%

:end
@rem End local scope for the variables with windows NT shell
if "%ERRORLEVEL%"=="0" goto mainEnd

:fail
rem Set variable HEXAGON_EXIT_CONSOLE if you need the _script_ return code instead of
rem the _cmd.exe /c_ return code!
if  not "" == "%HEXAGON_EXIT_CONSOLE%" exit 1
exit /b 1

:mainEnd
if "%OS%"=="Windows_NT" endlocal

:omega
