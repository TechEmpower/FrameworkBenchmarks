import os

from setuptools import setup, find_packages

requires = [
    'pyramid',
    'pyramid_chameleon',
    'psycopg2',
    'sqlalchemy',
    'gunicorn'
    ]

tests_require = [
  'nose-cov',
  'webtest'
]

setup(name='frameworkbenchmarks',
      version='0.0',
      description='FrameworkBenchmarks',
      classifiers=[
        "Programming Language :: Python",
        "Framework :: Pyramid",
        "Topic :: Internet :: WWW/HTTP",
        "Topic :: Internet :: WWW/HTTP :: WSGI :: Application",
        ],
      author='',
      author_email='',
      url='',
      keywords='web pyramid pylons',
      packages=find_packages(),
      include_package_data=True,
      zip_safe=False,
      install_requires=requires,
      tests_require=tests_require,
      test_suite="frameworkbenchmarks",
      entry_points="""\
      [paste.app_factory]
      main = frameworkbenchmarks:main
      """,
      )
