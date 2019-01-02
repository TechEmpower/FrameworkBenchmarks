# ActFramework Benchmarking Test

[ActFramework](https://github.com/actframework/actframework) Expressive, fast and testable web framework designed by Java developer for Java developer

This is the ActFramework portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

```java
public class AppEntry {

    @GetAction
    public String sayHelloTo(@DefaultValue("World") String who) {
        return "Hello " + who + "!";
    }

    public static void main(String[] args) throws Exception {
        Act.start();
    }

}
```

### Plain Text Test
* [Plain test source](src/main/resources/conf/json_plaintext/routes.conf)

### JSON Encoding Test
* [JSON test source](src/main/java/com/techempower/act/controller/HelloWorldController.java)

### Single Query Test
* [Single query test source](src/main/java/com/techempower/act/controller/WorldController.java)

### Multiple Queries Test
* [Multiple queries test source](src/main/java/com/techempower/act/controller/WorldController.java)

### Database Update Test
* [Database update test source](src/main/java/com/techempower/act/controller/WorldController.java)

### Fortunes Test
* [Fortunes test source](src/main/java/com/techempower/act/controller/FortuneController.java)
