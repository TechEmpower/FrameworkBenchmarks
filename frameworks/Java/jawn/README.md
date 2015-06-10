# Jawn framework benchmarking test
This is an implementation of [jawn](http://javapla.net)
as a portion of a [benchmarking test suite](../) comparing a variety 
of web development platforms.


## Running the application
If you have [gradle](http://gradle.org) installed:
```
gradle run
```
Or use the included wrapper:
```
./gradlew run
```
Point your browser at `localhost:8081/json`

The framework use an embedded server, that runs on port `8081` in order to
not interfere with potentially running webcontainers.

### A couple of commands to get going with the benchmark framework

toolset/run-tests.py --install server --test jawn --verbose --install-only

toolset/run-tests.py --test jawn --mode verify

toolset/run-tests.py --test jawn

toolset/run-tests.py --test jawn --type fortune --sleep 5


### Problems with the mounting of FrameworkBenchmarks?
* **On host**:     vagrant gem install vagrant-vbguest
* **In guest OS**: sudo mount.vboxsf -o uid=`id -u vagrant`,gid=`getent group vagrant | cut -d: -f3`,dmode=777,fmode=777 FrameworkBenchmarks /FrameworkBenchmarks