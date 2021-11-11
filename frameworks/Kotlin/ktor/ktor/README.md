# Ktor

Ktor is a framework for building servers and clients in connected systems using Kotlin programming language.
More information is available at [ktor.io](http://ktor.io). 

# Setup

* Java 8
* MySQL server

# Requirements

* Maven 3
* JDK 8
* Kotlin
* ktor
* netty 
* hikariCP

Maven is downloaded automatically via Maven Wrapper script (`mvnw`), add dependencies are specified in `pom.xml` so will be downloaded automatically from maven central and jcenter repositories.

# Deployment

Run maven to build a bundle

```bash
./mvnw package
```

Once bundle build complete and mysql server is running you can launch the application

```bash
java -jar target/tech-empower-framework-benchmark-1.0-SNAPSHOT.jar
```

Please note that the server holds tty so you may need nohup. See `setup.sh` for details.

# Contact

[Leonid Stashevsky](https://github.com/e5l)

[Sergey Mashkov](https://github.com/cy6erGn0m)

[Ilya Ryzhenkov](https://github.com/orangy) 

Slack ktor channel https://kotlinlang.slack.com/messages/ktor (you need an [invite](http://slack.kotlinlang.org/) to join)


