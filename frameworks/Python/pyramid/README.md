# Pyramid benchmark test

[Pyramid](http://www.pylonsproject.org/) is a flexible Python 2/3 framework.
This test uses [SQLAlchemy](http://www.sqlalchemy.org/) as its ORM, the default
[Chameleon](http://www.pylonsproject.org/) for its templating, and
[Gunicorn](https://github.com/benoitc/gunicorn) for the application server.

## Test URLs

### JSON Encoding

http://localhost:6543/json

### Single Row Random Query

http://localhost:6543/db

### Variable Row Query Test

http://localhost:6543/queries?queries=10

### Fortune Test

http://localhost:6543/fortunes

### Updates
http://localhost:6543/updates?queries=10

### Plaintext

http://localhost:6543/plaintext
