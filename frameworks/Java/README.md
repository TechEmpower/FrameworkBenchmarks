# Java Frameworks

The information below contains information specific to Java. 
For further guidance, review the 
[documentation](http://frameworkbenchmarks.readthedocs.org/en/latest/).

## Infrastructure Software Versions

* Java 7
* Java 8

## Adding a New Java Framework

### Installation and Bash Configuration

In order to declare that your framework requires Java, you 
should have an `install.sh` that contains at least

    #!/bin/bash

    fw_depends java

This installs the OpenJDK 7 JVM.

Frameworks can also choose to install Oracle Java 8 JVM by 
declaring a dependency on "java8" instead of java. In order 
to use Java 8 JVM frameworks need to add the following line 
in their "setup.sh" file:

    export JAVA_HOME=/opt/java8

## Get Help

### Java Experts

_There aren't any experts listed, yet. If you're an expert, 
add yourself!_

### Interesting Links

* [Surprise! Java is fastest for server-side Web apps](http://www.infoworld.com/article/2609675/java/surprise--java-is-fastest-for-server-side-web-apps.html)
