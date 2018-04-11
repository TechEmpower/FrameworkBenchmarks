import os
import glob
import json

from collections import OrderedDict

from toolset.utils.output_helper import log
from colorama import Fore


def gather_langauges(benchmarker_config):
    '''
    Gathers all the known languages in the suite via the folder names
    beneath FWROOT.
    '''

    lang_dir = os.path.join(benchmarker_config.fwroot, "frameworks")
    langs = []
    for dir in glob.glob(os.path.join(lang_dir, "*")):
        langs.append(dir.replace(lang_dir, "")[1:])
    return langs


def gather_tests(include=[], exclude=[], benchmarker_config=None,
                 results=None):
    '''
    Given test names as strings, returns a list of FrameworkTest objects.
    For example, 'aspnet-mysql-raw' turns into a FrameworkTest object with
    variables for checking the test directory, the test database os, and
    other useful items.

    With no arguments, every test in this framework will be returned.
    With include, only tests with this exact name will be returned.
    With exclude, all tests but those excluded will be returned.

    A config is needed to construct full FrameworkTest objects. If
    one is not provided, a default config will be created.
    '''

    # Help callers out a bit
    if include is None:
        include = []
    if exclude is None:
        exclude = []

    # Old, hacky method to exclude all tests was to
    # request a test known to not exist, such as ''.
    # If test '' was requested, short-circuit and return
    # nothing immediately
    if len(include) == 1 and '' in include:
        return []

    # Search for configuration files
    config_files = []

    if benchmarker_config.test_lang:
        benchmarker_config.test_dir = []
        for lang in benchmarker_config.test_lang:
            if os.path.exists("{!s}/frameworks/{!s}".format(
                    benchmarker_config.fwroot, lang)):
                for test_dir in os.listdir("{!s}/frameworks/{!s}".format(
                        benchmarker_config.fwroot, lang)):
                    benchmarker_config.test_dir.append("{!s}/{!s}".format(
                        lang, test_dir))
            else:
                raise Exception(
                    "Unable to locate language directory: {!s}".format(lang))

    if benchmarker_config.test_dir:
        for test_dir in benchmarker_config.test_dir:
            dir_config_files = glob.glob(
                "{!s}/frameworks/{!s}/benchmark_config.json".format(
                    benchmarker_config.fwroot, test_dir))
            if len(dir_config_files):
                config_files.extend(dir_config_files)
            else:
                raise Exception(
                    "Unable to locate tests in test-dir: {!s}".format(
                        test_dir))
    else:
        config_files.extend(
            glob.glob("{!s}/frameworks/*/*/benchmark_config.json".format(
                benchmarker_config.fwroot)))

    tests = []
    for config_file_name in config_files:
        config = None
        with open(config_file_name, 'r') as config_file:
            try:
                config = json.load(config_file)
            except ValueError:
                log("Error loading config: {!s}".format(config_file_name),
                    color=Fore.RED)
                raise Exception("Error loading config file")

        # Find all tests in the config file
        config_tests = parse_config(config, os.path.dirname(config_file_name),
                                    benchmarker_config, results)

        # Filter
        for test in config_tests:
            if len(include) is 0 and len(exclude) is 0:
                # No filters, we are running everything
                tests.append(test)
            elif test.name in exclude:
                continue
            elif test.name in include:
                tests.append(test)
            else:
                # An include list exists, but this test is
                # not listed there, so we ignore it
                pass

    # Ensure we were able to locate everything that was
    # explicitly included
    if 0 != len(include):
        names = {test.name for test in tests}
        if 0 != len(set(include) - set(names)):
            missing = list(set(include) - set(names))
            raise Exception("Unable to locate tests %s" % missing)

    tests.sort(key=lambda x: x.name)
    return tests


def gather_remaining_tests(config, results):
    '''
    Gathers the tests remaining in a current benchmark run.
    '''
    return gather_tests(config.test, config.exclude, config, results)


def gather_frameworks(include=[], exclude=[], config=None):
    '''
    Return a dictionary mapping frameworks->[test1,test2,test3]
    for quickly grabbing all tests in a grouped manner.
    Args have the same meaning as gather_tests
    '''
    tests = gather_tests(include, exclude, config)
    frameworks = dict()

    for test in tests:
        if test.framework not in frameworks:
            frameworks[test.framework] = []
        frameworks[test.framework].append(test)
    return frameworks


def test_order(type_name):
    """
    This sort ordering is set up specifically to return the length
    of the test name. There were SO many problems involved with
    'plaintext' being run first (rather, just not last) that we
    needed to ensure that it was run last for every framework.
    """
    return len(type_name)


def parse_config(config, directory, benchmarker_config, results):
    """
    Parses a config file into a list of FrameworkTest objects
    """
    from toolset.benchmark.framework_test import FrameworkTest
    tests = []

    # The config object can specify multiple tests
    # Loop over them and parse each into a FrameworkTest
    for test in config['tests']:

        tests_to_run = [name for (name, keys) in test.iteritems()]
        if "default" not in tests_to_run:
            log("Framework %s does not define a default test in benchmark_config.json"
                % config['framework'])

        # Check that each test configuration is acceptable
        # Throw exceptions if a field is missing, or how to improve the field
        for test_name, test_keys in test.iteritems():
            # Validates the benchmark_config entry
            validate_test(test_name, test_keys, directory)

            # Map test type to a parsed FrameworkTestType object
            runTests = dict()
            for type_name, type_obj in benchmarker_config.types.iteritems():
                try:
                    # Makes a FrameWorkTestType object using some of the keys in config
                    # e.g. JsonTestType uses "json_url"
                    runTests[type_name] = type_obj.copy().parse(test_keys)
                except AttributeError:
                    # This is quite common - most tests don't support all types
                    # Quitely log it and move on (debug logging is on in travis and this causes
                    # ~1500 lines of debug, so I'm totally ignoring it for now
                    # log("Missing arguments for test type %s for framework test %s" % (type_name, test_name))
                    pass

            # We need to sort by test_type to run
            sortedTestKeys = sorted(runTests.keys(), key=test_order)
            sortedRunTests = OrderedDict()
            for sortedTestKey in sortedTestKeys:
                sortedRunTests[sortedTestKey] = runTests[sortedTestKey]

            # Prefix all test names with framework except 'default' test
            # Done at the end so we may still refer to the primary test as `default` in benchmark config error messages
            if test_name == 'default':
                test_name = config['framework']
            else:
                test_name = "%s-%s" % (config['framework'], test_name)

            # By passing the entire set of keys, each FrameworkTest will have a member for each key
            tests.append(
                FrameworkTest(test_name, directory, benchmarker_config,
                              results, sortedRunTests, test_keys))

    return tests


def validate_test(test_name, test_keys, directory):
    """
    Validate benchmark config values for this test based on a schema
    """
    recommended_lang = directory.split('/')[-2]
    windows_url = "https://github.com/TechEmpower/FrameworkBenchmarks/issues/1038"
    schema = {
        'language': {
            'help':
            ('language', 'The language of the framework used, suggestion: %s' %
             recommended_lang)
        },
        'webserver': {
            'help':
            ('webserver',
             'Name of the webserver also referred to as the "front-end server"'
             )
        },
        'classification': {
            'allowed': [('Fullstack', '...'), ('Micro', '...'), ('Platform',
                                                                 '...')]
        },
        'database': {
            'allowed':
            [('MySQL',
              'One of the most popular databases around the web and in TFB'),
             ('Postgres',
              'An advanced SQL database with a larger feature set than MySQL'),
             ('MongoDB', 'A popular document-store database'),
             ('Cassandra', 'A highly performant and scalable NoSQL database'),
             ('Elasticsearch',
              'A distributed RESTful search engine that is used as a database for TFB tests'
              ),
             ('Redis',
              'An open-sourced, BSD licensed, advanced key-value cache and store'
              ),
             ('SQLite',
              'A network-less database, still supported for backwards compatibility'
              ), ('SQLServer', 'Microsoft\'s SQL implementation'),
             ('None',
              'No database was used for these tests, as is the case with Json Serialization and Plaintext'
              )]
        },
        'approach': {
            'allowed': [('Realistic', '...'), ('Stripped', '...')]
        },
        'orm': {
            'allowed':
            [('Full',
              'Has a full suite of features like lazy loading, caching, multiple language support, sometimes pre-configured with scripts.'
              ),
             ('Micro',
              'Has basic database driver capabilities such as establishing a connection and sending queries.'
              ),
             ('Raw',
              'Tests that do not use an ORM will be classified as "raw" meaning they use the platform\'s raw database connectivity.'
              )]
        },
        'platform': {
            'help':
            ('platform',
             'Name of the platform this framework runs on, e.g. Node.js, PyPy, hhvm, JRuby ...'
             )
        },
        'framework': {
            # Guranteed to be here and correct at this point
            # key is left here to produce the set of required keys
        },
        'os': {
            'allowed':
            [('Linux',
              'Our best-supported host OS, it is recommended that you build your tests for Linux hosts'
              ),
             ('Windows',
              'TFB is not fully-compatible on windows, contribute towards our work on compatibility: %s'
              % windows_url)]
        },
        'database_os': {
            'allowed':
            [('Linux',
              'Our best-supported host OS, it is recommended that you build your tests for Linux hosts'
              ),
             ('Windows',
              'TFB is not fully-compatible on windows, contribute towards our work on compatibility: %s'
              % windows_url)]
        }
    }

    # Confirm required keys are present
    required_keys = schema.keys()
    missing = list(set(required_keys) - set(test_keys))

    if len(missing) > 0:
        missingstr = (", ").join(map(str, missing))
        raise Exception(
            "benchmark_config.json for test %s is invalid, please amend by adding the following required keys: [%s]"
            % (test_name, missingstr))

    # Check the (all optional) test urls
    validate_urls(test_name, test_keys)

    # Check values of keys against schema
    for key in required_keys:
        val = test_keys.get(key, "").lower()
        has_predefined_acceptables = 'allowed' in schema[key]

        if has_predefined_acceptables:
            allowed = schema[key].get('allowed', [])
            acceptable_values, descriptors = zip(*allowed)
            acceptable_values = [a.lower() for a in acceptable_values]

            if val not in acceptable_values:
                msg = (
                    "Invalid `%s` value specified for test \"%s\" in framework \"%s\"; suggestions:\n"
                    % (key, test_name, test_keys['framework']))
                helpinfo = ('\n').join([
                    "  `%s` -- %s" % (v, desc)
                    for (v, desc) in zip(acceptable_values, descriptors)
                ])
                fullerr = msg + helpinfo + "\n"
                raise Exception(fullerr)

        elif not has_predefined_acceptables and val == "":
            msg = (
                "Value for `%s` in test \"%s\" in framework \"%s\" was missing:\n"
                % (key, test_name, test_keys['framework']))
            helpinfo = "  %s -- %s" % schema[key]['help']
            fullerr = msg + helpinfo + '\n'
            raise Exception(fullerr)


def validate_urls(test_name, test_keys):
    """
    Separated from validate_test because urls are not required anywhere. We know a url is incorrect if it is
    empty or does not start with a "/" character. There is no validation done to ensure the url conforms to
    the suggested url specifications, although those suggestions are presented if a url fails validation here.
    """
    example_urls = {
        "json_url":
        "/json",
        "db_url":
        "/mysql/db",
        "query_url":
        "/mysql/queries?queries=  or  /mysql/queries/",
        "fortune_url":
        "/mysql/fortunes",
        "update_url":
        "/mysql/updates?queries=  or  /mysql/updates/",
        "plaintext_url":
        "/plaintext",
        "cached_query_url":
        "/mysql/cached_queries?queries=  or /mysql/cached_queries"
    }

    for test_url in [
            "json_url", "db_url", "query_url", "fortune_url", "update_url",
            "plaintext_url", "cached_query_url"
    ]:
        key_value = test_keys.get(test_url, None)
        if key_value != None and not key_value.startswith('/'):
            errmsg = """`%s` field in test \"%s\" does not appear to be a valid url: \"%s\"\n
        Example `%s` url: \"%s\"
      """ % (test_url, test_name, key_value, test_url, example_urls[test_url])
            raise Exception(errmsg)
