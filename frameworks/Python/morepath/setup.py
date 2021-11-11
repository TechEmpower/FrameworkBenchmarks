# -*- coding: utf-8 -*-

from setuptools import setup, find_packages

setup(
    name='frameworkbenchmarks',
    version='0.0',
    description='FrameworkBenchmarks',
    author='',
    author_email='',
    url='',
    packages=find_packages(),
    include_package_data=True,
    zip_safe=False,
    platforms='any',
    install_requires=[
        'more.pony',
        'psycopg2',
        'more.jinja2',
        'gunicorn',
        'meinheld',
    ],
    extras_require=dict(
        test=[
            'pytest >= 2.9.1',
            'WebTest >= 2.0.14',
            'pytest-cov',
        ]
    ),
    entry_points=dict(
        morepath=[
            'scan = app',
        ],
    ),
    classifiers=[
        'Programming Language :: Python',
        'Framework :: Morepath',
        'Topic :: Internet :: WWW/HTTP',
        'Topic :: Internet :: WWW/HTTP :: WSGI :: Application',
    ]
)
