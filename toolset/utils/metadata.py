import os
import glob
import json

from collections import OrderedDict
from colorama import Fore
from toolset.databases import databases
from toolset.utils.output_helper import log


class Metadata:

    supported_dbs = []
    for name in databases:
        supported_dbs.append((name, '...'))

    def __init__(self, benchmarker=None):
        self.benchmarker = benchmarker

    def gather_languages(self):
        '''
        Gathers all the known languages in the suite via the folder names
        beneath FWROOT.
        '''

        lang_dir = os.path.join(self.benchmarker.config.lang_root)
        langs = []
        for dir in glob.glob(os.path.join(lang_dir, "*")):
            langs.append(dir.replace(lang_dir, "")[1:])
        return langs

    def gather_language_tests(self, language):
        '''
        Gathers all the test names from a known language
        '''
        try:
            dir = os.path.join(self.benchmarker.config.lang_root, language)
            tests = map(lambda x: os.path.join(language, x), os.listdir(dir))
            return filter(lambda x: os.path.isdir(
                os.path.join(self.benchmarker.config.lang_root, x)), tests)
        except Exception:
            raise Exception(
                "Unable to locate language directory: {!s}".format(language))

    def get_framework_config(self, test_dir):
        '''
        Gets a framework's benchmark_config from the given
        test directory
        '''
        dir_config_files = glob.glob("{!s}/{!s}/benchmark_config.json".format(
            self.benchmarker.config.lang_root, test_dir))
        if len(dir_config_files):
            return dir_config_files[0]
        else:
            raise Exception(
                "Unable to locate tests in test-dir: {!s}".format(test_dir))

    def gather_tests(self, include=None, exclude=None):
        '''
        Given test names as strings, returns a list of FrameworkTest objects.
        For example, 'aspnet-mysql-raw' turns into a FrameworkTest object with
        variables for checking the test directory, the test database os, and
        other useful items.

        With no arguments, every test in this framework will be returned.
        With include, only tests with this exact name will be returned.
        With exclude, all tests but those excluded will be returned.
        '''

        # Help callers out a bit
        include = include or []
        exclude = exclude or []

        # Search for configuration files
        config_files = []

        if self.benchmarker.config.test_lang:
            self.benchmarker.config.test_dir = []
            for lang in self.benchmarker.config.test_lang:
                self.benchmarker.config.test_dir.extend(
                    self.gather_language_tests(lang))

        if self.benchmarker.config.test_dir:
            for test_dir in self.benchmarker.config.test_dir:
                config_files.append(self.get_framework_config(test_dir))
        else:
            config_files.extend(
                glob.glob("{!s}/*/*/benchmark_config.json".format(
                    self.benchmarker.config.lang_root)))

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
            config_tests = self.parse_config(config,
                                             os.path.dirname(config_file_name))

            # Filter
            for test in config_tests:
                if hasattr(test, "tags"):
                    if "broken" in test.tags:
                        continue
                    if self.benchmarker.config.tag:
                        for t in self.benchmarker.config.tag:
                            if t in test.tags and test.name not in exclude:
                                tests.append(test)
                                break
                if len(include) > 0:
                    if test.name in include:
                        tests.append(test)
                elif test.name not in exclude and not self.benchmarker.config.tag:
                    tests.append(test)

        # Ensure we were able to locate everything that was
        # explicitly included
        if len(include):
            names = {test.name for test in tests}
            if len(set(include) - set(names)):
                missing = list(set(include) - set(names))
                raise Exception("Unable to locate tests %s" % missing)

        tests = list(set(tests))
        tests.sort(key=lambda x: x.name)

        return tests

    def tests_to_run(self):
        '''
        Gathers all tests for current benchmark run.
        '''
        return self.gather_tests(self.benchmarker.config.test,
                                 self.benchmarker.config.exclude)

    def gather_frameworks(self, include=None, exclude=None):
        '''
        Return a dictionary mapping frameworks->[test1,test2,test3]
        for quickly grabbing all tests in a grouped manner.
        Args have the same meaning as gather_tests
        '''
        tests = self.gather_tests(include, exclude)
        frameworks = dict()

        for test in tests:
            if test.framework not in frameworks:
                frameworks[test.framework] = []
            frameworks[test.framework].append(test)
        return frameworks

    def has_file(self, test_dir, filename):
        '''
        Returns True if the file exists in the test dir
        '''
        path = test_dir
        if not self.benchmarker.config.lang_root in path:
            path = os.path.join(self.benchmarker.config.lang_root, path)
        return os.path.isfile("{!s}/{!s}".format(path, filename))

    @staticmethod
    def test_order(type_name):
        """
        This sort ordering is set up specifically to return the length
        of the test name. There were SO many problems involved with
        'plaintext' being run first (rather, just not last) that we
        needed to ensure that it was run last for every framework.
        """
        return len(type_name)

    def parse_config(self, config, directory):
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
                    % config['framework'],
                    color=Fore.YELLOW)

            # Check that each test configuration is acceptable
            # Throw exceptions if a field is missing, or how to improve the field
            for test_name, test_keys in test.iteritems():
                # Validates and normalizes the benchmark_config entry
                test_keys = Metadata.validate_test(test_name, test_keys,
                                                   config['framework'], directory)

                # Map test type to a parsed FrameworkTestType object
                runTests = dict()

                # TODO: remove self.benchmarker.config.types
                for type_name, type_obj in self.benchmarker.config.types.iteritems():
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
                sortedTestKeys = sorted(
                    runTests.keys(), key=Metadata.test_order)
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
                    FrameworkTest(test_name, directory, self.benchmarker,
                                  sortedRunTests, test_keys))

        return tests

    def to_jsonable(self):
        '''
        Returns an array suitable for jsonification
        '''
        all_tests = self.gather_tests()
        return map(lambda test: {
            "project_name": test.project_name,
            "name": test.name,
            "approach": test.approach,
            "classification": test.classification,
            "database": test.database,
            "framework": test.framework,
            "language": test.language,
            "orm": test.orm,
            "platform": test.platform,
            "webserver": test.webserver,
            "os": test.os,
            "database_os": test.database_os,
            "display_name": test.display_name,
            "notes": test.notes,
            "versus": test.versus,
            "tags": hasattr(test, "tags") and test.tags or []
        }, all_tests)

    def list_test_metadata(self):
        '''
        Prints the metadata for all the available tests
        '''
        all_tests_json = json.dumps(self.to_jsonable())

        with open(
                os.path.join(self.benchmarker.results.directory,
                             "test_metadata.json"), "w") as f:
            f.write(all_tests_json)

    @staticmethod
    def validate_test(test_name, test_keys, project_name, directory):
        """
        Validate and normalizes benchmark config values for this test based on a schema
        """
        recommended_lang = directory.split('/')[-2]
        windows_url = "https://github.com/TechEmpower/FrameworkBenchmarks/issues/1038"
        schema = {
            'language': {
                # Language is the only key right now with no 'allowed' key that can't
                # have a "None" value
                'required':
                True,
                'help': ('language',
                         'The language of the framework used, suggestion: %s' %
                         recommended_lang)
            },
            'webserver': {
                'help':
                ('webserver',
                 'Name of the webserver also referred to as the "front-end server"'
                 )
            },
            'classification': {
                'allowed': [('Fullstack', '...'), ('Micro', '...'),
                            ('Platform', '...')]
            },
            'database': {
                'allowed':
                Metadata.supported_dbs +
                [('None',
                  'No database was used for these tests, as is the case with Json Serialization and Plaintext'
                  )]
            },
            'approach': {
                'allowed': [('Realistic', '...'), ('Stripped', '...')]
            },
            'orm': {
                'required_with':
                'database',
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
                # Guaranteed to be here and correct at this point
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
                'required_with':
                'database',
                'allowed':
                [('Linux',
                  'Our best-supported host OS, it is recommended that you build your tests for Linux hosts'
                  ),
                 ('Windows',
                  'TFB is not fully-compatible on windows, contribute towards our work on compatibility: %s'
                  % windows_url)]
            }
        }

        # Check the (all optional) test urls
        Metadata.validate_urls(test_name, test_keys)

        def get_test_val(k):
            return test_keys.get(k, "none").lower()

        def throw_incorrect_key(k, acceptable_values, descriptors):
            msg = (
                "`%s` is a required key for test \"%s\" in framework \"%s\"\n"
                % (k, test_name, project_name))
            if acceptable_values:
                msg = (
                    "Invalid `%s` value specified for test \"%s\" in framework \"%s\"; suggestions:\n"
                    % (k, test_name, project_name))
                helpinfo = ('\n').join([
                    "  `%s` -- %s" % (v, desc)
                    for (v, desc) in zip(acceptable_values, descriptors)
                ])
                msg = msg + helpinfo + "\n"

            raise Exception(msg)

        # Check values of keys against schema
        for key in schema.keys():
            val = get_test_val(key)
            test_keys[key] = val
            acceptable_values = None
            descriptors = None

            if 'allowed' in schema[key]:
                allowed = schema[key].get('allowed', [])
                acceptable_values, descriptors = zip(*allowed)
                acceptable_values = [a.lower() for a in acceptable_values]

            if val == "none":
                # incorrect if key requires a value other than none
                if schema[key].get('required', False):
                    throw_incorrect_key(key, acceptable_values, descriptors)
                # certain keys are only required if another key is not none
                if 'required_with' in schema[key]:
                    if get_test_val(schema[key]['required_with']) != "none":
                        throw_incorrect_key(key, acceptable_values, descriptors)

            # if we're here, the key needs to be one of the "allowed" values
            elif acceptable_values and val not in acceptable_values:
                throw_incorrect_key(key, acceptable_values, descriptors)

        test_keys['project_name'] = project_name

        return test_keys

    @staticmethod
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
            if key_value is not None and not key_value.startswith('/'):
                errmsg = """`%s` field in test \"%s\" does not appear to be a valid url: \"%s\"\n
            Example `%s` url: \"%s\"
          """ % (test_url, test_name, key_value, test_url,
                 example_urls[test_url])
                raise Exception(errmsg)
