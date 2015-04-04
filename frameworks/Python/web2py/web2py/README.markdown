## Readme

web2py is a free open source full-stack framework for rapid development of fast, scalable, secure and portable database-driven web-based applications. 

It is written and programmable in Python. LGPLv3 License

Learn more at http://web2py.com

## Google App Engine deployment

    cp examples/app.yaml ./
    cp handlers/gaehandler.py ./
    
Then edit ./app.yaml and replace "yourappname" with yourappname.

## Import about this GIT repo

An important part of web2py is the Database Abstraction Layer (DAL). In early 2015 this was decoupled into a separate code-base (PyDAL). In terms of git, it is a sub-module of the main repository.

The use of a sub-module requires a one-time use of the --recursive flag for git clone if you are cloning web2py from scratch.

    git clone --recursive https://github.com/web2py/web2py.git

If you have an existing repository, the commands below need to be executed at least once:

    git submodule update --init --recursive

If you have a folder gluon/dal you must remove it:

    rm -r gluon/dal

PyDAL uses a separate stable release cycle to the rest of web2py. PyDAL releases will use a date-naming scheme similar to Ubuntu. Issues related to PyDAL should be reported to its separate repository.


## Documentation (readthedocs.org)

[![Docs Status](https://readthedocs.org/projects/web2py/badge/?version=latest&style=flat-square)](http://web2py.rtfd.org/)

## Tests

[![Build Status](https://img.shields.io/travis/web2py/web2py.svg?style=flat-square)](https://travis-ci.org/web2py/web2py)

[![Coverage Status](https://img.shields.io/coveralls/web2py/web2py.svg?style=flat-square)](https://coveralls.io/r/web2py/web2py)

## Installation Instructions

To start web2py there is NO NEED to install it. Just unzip and do:

    python web2py.py

That's it!!!

## web2py directory structure

    project/
        README
        LICENSE
        VERSION                    > this web2py version
        web2py.py                  > the startup script
        anyserver.py               > to run with third party servers
        ...                        > other handlers and example files
        gluon/                     > the core libraries
            packages/              > web2py submodules
              dal/
            contrib/               > third party libraries
            tests/                 > unittests  
        applications/              > are the apps
            admin/                 > web based IDE
                ...
            examples/              > examples, docs, links
                ...
            welcome/               > the scaffolding app (they all copy it)
                ABOUT
                LICENSE
                models/
                views/
                controllers/
                sessions/
                errors/
                cache/
                static/
                uploads/
                modules/
                cron/
                tests/
            ...                    > your own apps
        examples/                  > example config files, mv .. and customize
        extras/                    > other files which are required for building web2py
        scripts/                   > utility and installation scripts
        handlers/
            wsgihandler.py         > handler to connect to WSGI
            ...                    > handlers for Fast-CGI, SCGI, Gevent, etc
        site-packages/             > additional optional modules
        logs/                      > log files will go in there
        deposit/                   > a place where web2py stores apps temporarily

## Issues?

Report issues at https://github.com/web2py/web2py/issues
