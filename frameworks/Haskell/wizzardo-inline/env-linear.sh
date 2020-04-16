. $(pwd)/set-java-home.sh

export C_INCLUDE_PATH="$JAVA_HOME/include:$JAVA_HOME/include/linux"
export LIBRARY_PATH="$JAVA_HOME/jre/lib/amd64/server:$JAVA_HOME/lib/server"
export LD_LIBRARY_PATH="$LIBRARY_PATH"
export STACK_YAML=stack-linear.yaml
