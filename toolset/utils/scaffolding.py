# -*- coding: utf-8 -*-
import os, re
from shutil import copytree
from toolset.utils.metadata import Metadata


class Scaffolding:
    def __init__(self, benchmarker):
        print("""
-------------------------------------------------------------------------------
    This wizard is intended to help build the scaffolding required for a new 
    test to be benchmarked.

    From here, you will be prompted for values related to the test you
    wish to add.
-------------------------------------------------------------------------------"""
              )

        self.benchmarker = benchmarker
        self.benchmarker_config = benchmarker.config

        try:
            self.__gather_display_name()
            self.__gather_language()
            self.__gather_approach()
            self.__gather_classification()
            self.__gather_database()
            self.__gather_orm()
            self.__gather_webserver()
            self.__gather_versus()
            self.__confirm_values()
            self.__print_success()
        except:
            print("")

    def __gather_display_name(self):
        print("""
  The name of your test as you wish it to be displayed on the results page.

  Example: Gemini, Gin, Express
    """)
        self.__prompt_display_name()
        while not self.display_name:
            self.__prompt_display_name()
        self.name = self.display_name.lower()

    def __prompt_display_name(self):
        self.display_name = raw_input("Name: ").strip()

        found = False
        for framework in self.benchmarker.metadata.gather_frameworks():
            if framework.lower() == self.display_name.lower():
                found = True

        if found:
            print("""
  It appears that there is already a '%s' framework in the test suite. You will
  have to pick a different name.
      """ % self.display_name)
            self.display_name = None

    def __gather_language(self):
        print("""
  The language in which your test implementation is written.

  Example: Java, Go, PHP
    """)
        self.language = None
        while not self.language:
            self.__prompt_language()

    def __prompt_language(self):
        self.language = raw_input("Language: ").strip()

        known_languages = self.benchmarker.metadata.gather_languages()
        language = None
        for lang in known_languages:
            if lang.lower() == self.language.lower():
                language = lang

        if not language:
            similar = []
            for lang in known_languages:
                if lang.lower()[:1] == self.language.lower()[:1]:
                    similar.append(lang)
            similar = ', '.join(similar)

            print("""
  That language is not currently in our list of known languages.
  
  Here is a list of similar languages present in our benchmark suite that you
  may have meant:

  %s
      
  Did you mean to add the new language, '%s', to the benchmark suite?
      """ % (similar, self.language))
            valid = self.__prompt_confirm_new_language()
            while not valid:
                valid = self.__prompt_confirm_new_language()

            if self.confirm_new_lang == 'n':
                self.language = None
            else:
                self.language = self.language.title()

        return self.language

    def __prompt_confirm_new_language(self):
        self.confirm_new_lang = raw_input("Create New Language '%s' (y/n): " %
                                          self.language).strip().lower()
        return self.confirm_new_lang == 'y' or self.confirm_new_lang == 'n'

    def __gather_approach(self):
        print("""
  The approach of your test implementation.

  1) Realistic: Uses the framework with most out-of-the-box functionality 
                enabled. We consider this realistic because most applications 
                built with the framework will leave these features enabled.
  2) Stripped:  Removes or outright avoids implementing features that are
                unnecessary for the particulars of the benchmark exercise. This
                might illuminate the marginal improvement available in fine-
                tuning a framework to your application's use-case.

  Note: If you are unsure, then your approach is probably Realistic. The
        Stripped approach is seldom used and will not have results displayed
        by default on the results website.
    """)
        valid = self.__prompt_approach()
        while not valid:
            valid = self.__prompt_approach()

    def __prompt_approach(self):
        self.approach = raw_input("Approach [1/2]: ").strip()
        if self.approach == '1':
            self.approach = 'Realistic'
        if self.approach == '2':
            self.approach = 'Stripped'
        return self.approach == 'Realistic' or self.approach == 'Stripped'

    def __gather_classification(self):
        print("""
  The classification of your test implementation.

  1) Fullstack: Robust framework expected to provide high-level functionality 
                for serving as a web application; for example, ability to 
                compose views, provide functions for responding with several 
                data types (json, html, etc), connecting to a database, form 
                processing, etc.
  2) Micro:     Simple framework expected to provide enough middleware to build
                a robust web application such as request routing and some 
                simple plumbing, but may not include built-in functionality 
                such as, for example, server-composed views.
  3) Platform:  Barebones infrastructure for servicing HTTP requests, but does
                not include a framework at all.
    """)
        valid = self.__prompt_classification()
        while not valid:
            valid = self.__prompt_classification()
        if self.classification == 'Platform':
            self.platform = 'None'
            self.framework = 'None'
        else:
            self.framework = self.display_name
            self.__gather_platform()

    def __prompt_classification(self):
        self.classification = raw_input("Classification [1/2/3]: ").strip()
        if self.classification == '1':
            self.classification = 'Fullstack'
        if self.classification == '2':
            self.classification = 'Micro'
        if self.classification == '3':
            self.classification = 'Platform'
        return self.classification == 'Fullstack' or \
               self.classification == 'Micro' or \
               self.classification == 'Platform'

    def __gather_platform(self):
        print("""
  The platform of your test implementation.

  The platform is the low-level software or API used to host web applications 
  for the framework; the platform provides an implementation of the HTTP
  fundamentals.

  Not all frameworks have a platform and if your programming language provides
  much of that by which we define a platform, leave blank.

  Example: Servlet, Wai, .NET
    """)
        self.__prompt_platform()

    def __prompt_platform(self):
        self.platform = raw_input("Platform (optional): ").strip()
        if self.platform == '':
            self.platform = 'None'

    def __gather_database(self):
        print("""
  Which database will you be using for your test?
    """)
        i = 1
        prompt = "Database ["
        options = []
        for db in Metadata.supported_dbs:
            print("  {!s}) {!s}".format(i, db[0]))
            prompt += "{!s}/".format(i)
            options.append(db[0])
            i += 1
        print("  {!s}) None: No database at this time{!s}".format(
            i, os.linesep))
        prompt += "{!s}]: ".format(i)
        options.append("None")
        valid = self.__prompt_database(prompt, options)
        while not valid:
            valid = self.__prompt_database(prompt, options)

    def __prompt_database(self, prompt, options):
        self.database = raw_input(prompt).strip()
        if 0 < int(self.database) <= len(options):
            self.database = options[int(self.database) - 1]
            return True
        else:
            return False

    def __gather_orm(self):
        if self.database == 'None':
            self.orm = 'None'
            return

        print("""
  How you would classify the ORM (object relational mapper) of your test?

  1) Full:  A feature-rich ORM which provides functionality for interacting 
            with a database without writing a query in all but the most edge 
            cases.
  2) Micro: An ORM which provides functionality for interacting with a database
            for many trivial operations (querying, updating), but not more 
            robust cases (for example, gathering relations).
  3) Raw:   No ORM; raw database access.
    """)
        valid = self.__prompt_orm()
        while not valid:
            valid = self.__prompt_orm()

    def __prompt_orm(self):
        self.orm = raw_input("ORM [1/2/3]: ").strip()
        if self.orm == '1':
            self.orm = 'Full'
        if self.orm == '2':
            self.orm = 'Micro'
        if self.orm == '3':
            self.orm = 'Raw'
        return self.orm == 'Full' or \
               self.orm == 'Micro' or \
               self.orm == 'Raw'

    def __gather_webserver(self):
        print("""
  Name of the front-end webserver sitting in front of your test implementation.

  Your test implementation may not use a web-server and may act as its own; you
  can leave this blank in this case.

  Example: nginx, Meinheld, httplight
    """)
        self.__prompt_webserver()

    def __prompt_webserver(self):
        self.webserver = raw_input("Webserver (optional): ").strip()
        if self.webserver == '':
            self.webserver = 'None'

    def __gather_versus(self):
        print("""
  The name of another test (elsewhere in this project) that is a subset of this
  framework.
  This allows for the generation of the framework efficiency chart in the 
  results web site.
  For example, Compojure is compared to "servlet" since Compojure is built on 
  the Servlet platform.

  Example: Servlet, Wai, Undertow
    """)
        self.__prompt_versus()

    def __prompt_versus(self):
        self.versus = raw_input("Versus (optional): ").strip()
        if self.versus == '':
            self.versus = 'None'

    def __confirm_values(self):
        print("""
    Name: %s
    Language: %s
    Approach: %s
    Classification: %s
    Platform: %s
    ORM: %s
    Webserver: %s
    Versus: %s

  Finalize the initialization of your test given the above values?

  Note: once you have initialized your test, you can change these values later.
    """ % (self.display_name, self.language, self.approach,
           self.classification, self.platform, self.orm, self.webserver,
           self.versus))

        valid = self.__prompt_confirmation()
        while not valid:
            valid = self.__prompt_confirmation()

        if self.confirmation == 'y':
            self.__build_scaffolding()
        else:
            print('Aborting')

    def __prompt_confirmation(self):
        self.confirmation = raw_input("Initialize [y/n]: ").strip().lower()
        return self.confirmation == 'y' or self.confirmation == 'n'

    def __build_scaffolding(self):
        if self.__create_test_folder():
            self.__copy_scaffold_files()
            self.__edit_scaffold_files()

    def __create_test_folder(self):
        self.language_dir = os.path.join(self.benchmarker_config.lang_root,
                                         self.language)
        self.test_dir = os.path.join(self.language_dir, self.name)

        if os.path.exists(self.test_dir):
            print("Test '%s' already exists; aborting." % self.name)
            return False

        return True

    def __copy_scaffold_files(self):
        copytree(self.benchmarker_config.scaffold_root, self.test_dir)

    def __edit_scaffold_files(self):
        for file in os.listdir(os.path.join(self.test_dir)):
            self.__replace_text(
                os.path.join(self.test_dir, file), "\$NAME", self.name)
            self.__replace_text(
                os.path.join(self.test_dir, file), "\$DISPLAY_NAME",
                self.display_name)
            self.__replace_text(
                os.path.join(self.test_dir, file), "\$APPROACH", self.approach)
            self.__replace_text(
                os.path.join(self.test_dir, file), "\$CLASSIFICATION",
                self.classification)
            self.__replace_text(
                os.path.join(self.test_dir, file), "\$FRAMEWORK",
                self.framework)
            self.__replace_text(
                os.path.join(self.test_dir, file), "\$LANGUAGE", self.language)
            self.__replace_text(
                os.path.join(self.test_dir, file), "\$DATABASE", self.database)
            self.__replace_text(
                os.path.join(self.test_dir, file), "\$ORM", self.orm)
            self.__replace_text(
                os.path.join(self.test_dir, file), "\$PLATFORM", self.platform)
            self.__replace_text(
                os.path.join(self.test_dir, file), "\$WEBSERVER",
                self.webserver)
            self.__replace_text(
                os.path.join(self.test_dir, file), "\$VERSUS", self.versus)

    def __print_success(self):
        print("""
-------------------------------------------------------------------------------
  Success!

  Your new test structure has been built to the specifications of the suite.
  Here is a brief run-down of what has been built:

    frameworks
        └─── %s
              └─── %s
                    ├─── benchmark_config.json
                    ├─── README.md

  The next step is to read through your README.md and follow the instructions
  provided therein.
-------------------------------------------------------------------------------"""
              % (self.language, self.name))

    # Replaces all text found using the regular expression to_replace with the supplied replacement.
    def __replace_text(self, file, to_replace, replacement):
        with open(file, "r") as conf:
            contents = conf.read()
        replaced_text = re.sub(to_replace, replacement, contents)
        with open(file, "w") as f:
            f.write(replaced_text)
