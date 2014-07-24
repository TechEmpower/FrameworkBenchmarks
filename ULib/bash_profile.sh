if [ -z "${FWROOT}" ]; then
  export FWROOT=${HOME}/FrameworkBenchmarks
fi
#------------------------------------------------------------ --------------------------------------------
# IROOT - Path of this test's install directory ($FWROOT/installs or $FWROOT/installs/pertest/<test-name>)
#------------------------------------------------------------ --------------------------------------------
if [ -z "${IROOT}" ]; then
  export IROOT=${FWROOT}/installs
fi

# Set the root of our ULib installation
export ULIB_ROOT=${IROOT}/ULib

# Where to find the userver_tcp executable
export PATH="${ULIB_ROOT}/bin:$PATH"

export ULIB_VERSION=1.4.1
export ULIB_DOCUMENT_ROOT=${ULIB_ROOT}/ULIB_DOCUMENT_ROOT
export ULIB_BUILD_OUTPUT=${ULIB_ROOT}/ULIB_BUILD_OUTPUT.txt
export ULIB_SERVER_OUTPUT=${ULIB_ROOT}/ULIB_SERVER_OUTPUT.txt
