# Jawn framework benchmarking test
This is an implementation of [jawn](http://javapla.net)
as a portion of a [benchmarking test suite](../) comparing a variety 
of web development platforms.


## Running the application
Have [gradle](http://gradle.org) installed, then:
```
gradle run
```

Point your browser at `localhost:8080/json`

### A couple of commands to get going with the benchmark framework

tfb --test jawn

tfb --sleep 5 --duration 10 --clean --test jawn

tfb --sleep 5 --type fortune --test jawn
