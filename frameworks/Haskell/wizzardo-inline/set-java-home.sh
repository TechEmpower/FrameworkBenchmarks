dir="$(java -XshowSettings:properties -version 2>&1 > /dev/null | grep 'java.home' | sed 's/[[:space:]]*java\.home = //')"

if [ -d $dir/include ]
then
  export JAVA_HOME="$dir"
else
  export JAVA_HOME="$(dirname "$dir")"
fi
