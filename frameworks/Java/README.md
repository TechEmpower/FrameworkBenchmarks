# Installation and Bash Configuration

In order to declare that your framework requires Java, you should have an `install.sh`
that contains at least

    #!/bin/bash

    fw_depends java

This installs the OpenJDK 7 JVM.

Frameworks can also choose to install Oracle Java 8 JVM by declaring a dependency on "java8"
instead of java. In order to use Java 8 JVM frameworks need to add the following line in their "setup.sh" file:

export JAVA_HOME=/opt/java8
