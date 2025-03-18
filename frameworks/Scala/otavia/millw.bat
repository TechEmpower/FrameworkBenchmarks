@echo off

rem This is a wrapper script, that automatically download mill from GitHub release pages
rem You can give the required mill version with --mill-version parameter
rem If no version is given, it falls back to the value of DEFAULT_MILL_VERSION
rem
rem Project page: https://github.com/lefou/millw
rem Script Version: 0.4.6
rem
rem If you want to improve this script, please also contribute your changes back!
rem
rem Licensed under the Apache License, Version 2.0

rem setlocal seems to be unavailable on Windows 95/98/ME
rem but I don't think we need to support them in 2019
setlocal enabledelayedexpansion

if [!DEFAULT_MILL_VERSION!]==[] (
    set "DEFAULT_MILL_VERSION=0.10.10"
)

if [!GITHUB_RELEASE_CDN!]==[] (
    set "GITHUB_RELEASE_CDN="
)

set "MILL_REPO_URL=https://github.com/com-lihaoyi/mill"

rem %~1% removes surrounding quotes
if [%~1%]==[--mill-version] (
  if not [%~2%]==[] (
    set MILL_VERSION=%~2%
    rem shift command doesn't work within parentheses
    set "STRIP_VERSION_PARAMS=true"
  ) else (
    echo You specified --mill-version without a version. 1>&2
    echo Please provide a version that matches one provided on 1>&2
    echo %MILL_REPO_URL%/releases 1>&2
    exit /b 1
  )
)

if not defined STRIP_VERSION_PARAMS GOTO AfterStripVersionParams
rem strip the: --mill-version {version}
shift
shift
:AfterStripVersionParams

if [!MILL_VERSION!]==[] (
  if exist .mill-version (
      set /p MILL_VERSION=<.mill-version
  ) else (
    if exist .config\mill-version (
      set /p MILL_VERSION=<.config\mill-version
    )
  )
)

if [!MILL_VERSION!]==[] (
    set MILL_VERSION=%DEFAULT_MILL_VERSION%
)

set MILL_DOWNLOAD_PATH=%USERPROFILE%\.mill\download

rem without bat file extension, cmd doesn't seem to be able to run it
set MILL=%MILL_DOWNLOAD_PATH%\!MILL_VERSION!.bat

if not exist "%MILL%" (
    set VERSION_PREFIX=%MILL_VERSION:~0,4%
    set DOWNLOAD_SUFFIX=-assembly
    if [!VERSION_PREFIX!]==[0.0.] set DOWNLOAD_SUFFIX=
    if [!VERSION_PREFIX!]==[0.1.] set DOWNLOAD_SUFFIX=
    if [!VERSION_PREFIX!]==[0.2.] set DOWNLOAD_SUFFIX=
    if [!VERSION_PREFIX!]==[0.3.] set DOWNLOAD_SUFFIX=
    if [!VERSION_PREFIX!]==[0.4.] set DOWNLOAD_SUFFIX=
    set VERSION_PREFIX=

    for /F "delims=- tokens=1" %%A in ("!MILL_VERSION!") do set MILL_VERSION_BASE=%%A
    for /F "delims=- tokens=2" %%A in ("!MILL_VERSION!") do set MILL_VERSION_MILESTONE=%%A
	set VERSION_MILESTONE_START=!MILL_VERSION_MILESTONE:~0,1!
    if [!VERSION_MILESTONE_START!]==[M] (
        set MILL_VERSION_TAG="!MILL_VERSION_BASE!-!MILL_VERSION_MILESTONE!"
    ) else (
        set MILL_VERSION_TAG=!MILL_VERSION_BASE!
    )

    rem there seems to be no way to generate a unique temporary file path (on native Windows)
    set DOWNLOAD_FILE=%MILL%.tmp

    set DOWNLOAD_URL=!GITHUB_RELEASE_CDN!%MILL_REPO_URL%/releases/download/!MILL_VERSION_TAG!/!MILL_VERSION!!DOWNLOAD_SUFFIX!

    echo Downloading mill %MILL_VERSION% from !DOWNLOAD_URL! ... 1>&2

    if not exist "%MILL_DOWNLOAD_PATH%" mkdir "%MILL_DOWNLOAD_PATH%"
    rem curl is bundled with recent Windows 10
    rem but I don't think we can expect all the users to have it in 2019
    where /Q curl
    if %ERRORLEVEL% EQU 0 (
        curl -f -L "!DOWNLOAD_URL!" -o "!DOWNLOAD_FILE!"
    ) else (
        rem bitsadmin seems to be available on Windows 7
        rem without /dynamic, github returns 403
        rem bitsadmin is sometimes needlessly slow but it looks better with /priority foreground
        bitsadmin /transfer millDownloadJob /dynamic /priority foreground "!DOWNLOAD_URL!" "!DOWNLOAD_FILE!"
    )
    if not exist "!DOWNLOAD_FILE!" (
        echo Could not download mill %MILL_VERSION% 1>&2
        exit /b 1
    )

    move /y "!DOWNLOAD_FILE!" "%MILL%"

    set DOWNLOAD_FILE=
    set DOWNLOAD_SUFFIX=
)

set MILL_DOWNLOAD_PATH=
set MILL_VERSION=
set MILL_REPO_URL=

if [!MILL_MAIN_CLI!]==[] (
    set "MILL_MAIN_CLI=%0"
)

rem Need to preserve the first position of those listed options
set MILL_FIRST_ARG=
if [%~1%]==[--bsp] (
  set MILL_FIRST_ARG=%1%
) else (
  if [%~1%]==[-i] (
    set MILL_FIRST_ARG=%1%
  ) else (
    if [%~1%]==[--interactive] (
      set MILL_FIRST_ARG=%1%
    ) else (
      if [%~1%]==[--no-server] (
        set MILL_FIRST_ARG=%1%
      ) else (
        if [%~1%]==[--repl] (
          set MILL_FIRST_ARG=%1%
        ) else (
          if [%~1%]==[--help] (
            set MILL_FIRST_ARG=%1%
          )
        )
      )
    )
  )
)

set "MILL_PARAMS=%*%"

if not [!MILL_FIRST_ARG!]==[] (
  if defined STRIP_VERSION_PARAMS (
    for /f "tokens=1-3*" %%a in ("%*") do (
        set "MILL_PARAMS=%%d"
    )
  ) else (
    for /f "tokens=1*" %%a in ("%*") do (
      set "MILL_PARAMS=%%b"
    )
  )
) else (
  if defined STRIP_VERSION_PARAMS (
    for /f "tokens=1-2*" %%a in ("%*") do (
        rem strip %%a - It's the "--mill-version" option.
        rem strip %%b - it's the version number that comes after the option.
        rem keep  %%c - It's the remaining options.
        set "MILL_PARAMS=%%c"
    )
  )
)

"%MILL%" %MILL_FIRST_ARG% -D "mill.main.cli=%MILL_MAIN_CLI%" %MILL_PARAMS%
