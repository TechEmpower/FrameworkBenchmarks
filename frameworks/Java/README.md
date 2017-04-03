# Java Frameworks

The information below contains information specific to Java. 
For further guidance, review the 
[documentation](http://frameworkbenchmarks.readthedocs.org/en/latest/).

## Infrastructure Software Versions

* Java 8

## Adding a New Java Framework

### Installation and Bash Configuration

In order to declare that your framework requires Java, you 
should have an `install.sh` that contains at least

    #!/bin/bash

    fw_depends java

This installs the Oracle Java 8 JDK. It also provides a shell variable `JAVA_OPTS` with tweaked default parameters for the JVM. Sample use:

    #!/bin/bash

    java -server $JAVA_OPTS

## Get Help

### Java Experts

_There aren't any experts listed, yet. If you're an expert, 
add yourself!_

### Interesting Links

* [Surprise! Java is fastest for server-side Web apps](http://www.infoworld.com/article/2609675/java/surprise--java-is-fastest-for-server-side-web-apps.html)
