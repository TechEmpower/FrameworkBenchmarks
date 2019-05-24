# delight.im 'delight-im/foundation'
[![Latest Stable Version](https://poser.pugx.org/delight-im/foundation-core/v/stable)](https://packagist.org/packages/delight-im/foundation-core)
[![Total Downloads](https://poser.pugx.org/delight-im/foundation-core/downloads)](https://packagist.org/packages/delight-im/foundation-core)
[![Latest Unstable Version](https://poser.pugx.org/delight-im/foundation-core/v/unstable)](https://packagist.org/packages/delight-im/foundation-core)
[![License](https://poser.pugx.org/delight-im/foundation-core/license)](https://packagist.org/packages/delight-im/foundation-core)
[![Monthly Downloads](https://poser.pugx.org/delight-im/foundation-core/d/monthly)](https://packagist.org/packages/delight-im/foundation-core)

# About delight.im 'delight-im/foundation'
**Writing modern PHP applications efficiently**

Build *better* applications *faster*, while still using “plain old PHP” only.

There are no DSLs or pseudo-languages that you need to learn (except [Twig](views/welcome.html)!), nor any “magical” command-line utilities.

[Github repository and documentation](https://github.com/delight-im/PHP-Foundation)

### Test Type Implementation Source Code

* [JSON](/app/Controller.php)
* [PLAINTEXT](/app/Controller.php)
* [DB](/app/Controller.php)
* [QUERY](/app/Controller.php)
* [UPDATE](/app/Controller.php)
* [FORTUNES](/app/Controller.php)

## Test URLs
### JSON

http://localhost:8080/json

### PLAINTEXT

http://localhost:8080/plaintext

### DB

http://localhost:8080/db

### QUERY

http://localhost:8080/queries/[count]

### UPDATE

http://localhost:8080/updates/[count]

### FORTUNES

http://localhost:8080/fortunes

##Notes about test implementation
* Using modification to App component to lazily-initialize authentication, which provides direct comparison to other frameworks in this benchmark and avoids session handling and auth overhead on routes that do not require authentication.
* Follows the discussion in https://github.com/delight-im/PHP-DB/issues/2 for creating a persistent PDO connection (PDO::ATTR_PERSISTENT)

benchmark test questions: [Brion Finlay](https://github.com/bfinlay)