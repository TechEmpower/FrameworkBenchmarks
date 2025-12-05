# Ktor

Ktor is a framework for building servers and clients in connected systems using Kotlin programming language.
More information is available at [ktor.io](http://ktor.io). 

# Setup

* Java 21
* Postgres server

# Requirements

* JDK 21
* Gradle (wrapper provided)
* Kotlin
* ktor
* netty 
* hikariCP

# Deployment

Use the Gradle wrapper to assemble the desired runnable bundle (Netty shown below).

```bash
./gradlew nettyBundle
```

Once the bundle build completes and Postgres is running you can launch the application

```bash
java -jar build/libs/tech-empower-framework-benchmark-1.0-SNAPSHOT-netty-bundle.jar
```

Please note that the server holds tty so you may need nohup. See `setup.sh` for details.

# Contact

[Leonid Stashevsky](https://github.com/e5l)

[Bruce Hamilton](https://github.com/bjhham)

[Sergey Mashkov](https://github.com/cy6erGn0m)

[Ilya Ryzhenkov](https://github.com/orangy) 

Slack ktor channel https://kotlinlang.slack.com/messages/ktor (you need an [invite](http://slack.kotlinlang.org/) to join)


