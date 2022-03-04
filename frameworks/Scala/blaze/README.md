#blaze Benchmarking Test

## Test URLs
### JSON Encoding Test

http://localhost:8080/json

### Plaintext Test

http://localhost:8080/plaintext

## How to run
sbt assembly

java -server -Xms2g -Xmx2g -XX:NewSize=1g -XX:MaxNewSize=1g -XX:InitialCodeCacheSize=256m -XX:ReservedCodeCacheSize=256m -XX:+UseParallelGC -XX:+UseNUMA -XX:+AggressiveOpts -XX:-UseBiasedLocking -XX:+AlwaysPreTouch -jar target/scala-2.12/blaze-assembly-1.0.jar

