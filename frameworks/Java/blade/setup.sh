mvn clean package		  mvn clean package
  cd target/dist/hello-blade-1.0.0-BUILD-SNAPSHOT		  cd target/dist/hello-blade-1.0.0-BUILD-SNAPSHOT
  		  
 java -Xms2G -Xmx2G -server -XX:+UseNUMA -XX:+UseParallelGC -XX:+AggressiveOpts -jar hello-blade-1.0.0-BUILD-SNAPSHOT.jar & 
