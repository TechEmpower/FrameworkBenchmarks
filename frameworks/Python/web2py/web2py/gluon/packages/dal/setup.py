"""
pyDAL is a pure Python Database Abstraction Layer.

It dynamically generates the SQL in real time using the specified dialect for
the database back end, so that you do not have to write SQL code or learn
different SQL dialects (the term SQL is used generically), and your code will
be portable among different types of databases.

pyDAL comes from the original web2py's DAL, with the aim of being
wide-compatible. pyDAL doesn't require web2py and can be used in any
Python context.


Links
-----
* `website <https://github.com/web2py/pydal>`_
* `documentation <http://www.web2py.com/books/default/chapter/29/06/the-database-abstraction-layer>`_
"""

from setuptools import setup
setup(
    name='pyDAL',
    version='15.03',
    url='https://github.com/web2py/pydal',
    license='BSD',
    author='Massimo Di Pierro',
    author_email='mdipierro@cs.depaul.edu',
    maintainer='Giovanni Barillari',
    maintainer_email='gi0baro@d4net.org',
    description='a pure Python Database Abstraction Layer',
    long_description=__doc__,
    packages=['pydal', 'pydal.adapters', 'pydal.helpers', 'pydal.contrib',
              'pydal.contrib.simplejson'],
    include_package_data=True,
    zip_safe=False,
    platforms='any',
    classifiers=[
        'Development Status :: 5 - Production/Stable',
        'Environment :: Web Environment',
        'Intended Audience :: Developers',
        'License :: OSI Approved :: BSD License',
        'Operating System :: OS Independent',
        'Programming Language :: Python',
        'Programming Language :: Python :: 2',
        'Topic :: Database :: Front-Ends',
        'Topic :: Software Development :: Libraries :: Python Modules'
    ]
)
