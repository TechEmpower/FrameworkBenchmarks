import sys
import os
import subprocess
from shutil import copytree
from setup_util import replace_text

class Scaffolding:
  def __init__(self):
    print("""
-------------------------------------------------------------------------------
    This wizard is intended to help build the scaffolding required for a new 
    test to be benchmarked.

    From here, you will be prompted for values related to the test you
    wish to add.
-------------------------------------------------------------------------------""")

    try:
      self.__gather_display_name()
      self.__gather_language()
      self.__gather_approach()
      self.__gather_classification()
      self.__gather_orm()
      self.__gather_webserver()
      self.__gather_versus()

      self.__confirm_values()
    except:
      print("")

  def __gather_display_name(self):
    print("""
  The name of your test as you wish it to be displayed on the results page.
    """)
    self.__prompt_display_name()
    while not self.display_name:
      self.__prompt_display_name()
    self.name = self.display_name.lower()

  def __prompt_display_name(self):
    self.display_name = raw_input("Name: ").strip()

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
    return self.language

  def __gather_approach(self):
    print("""
  The approach of your test implementation.

  Realistic: Uses the framework with most out-of-the-box functionality enabled.
             We consider this realistic because most applications built with 
             the framework will leave these features enabled.
  Stripped:  Removes or outright avoids implementing features that are
             unnecessary for the particulars of the benchmark exercise. This
             might illuminate the marginal improvement available in fine-
             tunning a framework to your application's use-case.

  Note: If you are unsure, then your approach is probably Realistic. The
        Stripped approach is seldom used and will not have results displayed
        by default on the results website.
    """)
    valid = self.__prompt_approach()
    while not valid:
      valid = self.__prompt_approach()

  def __prompt_approach(self):
    self.approach = raw_input("Approach [Realistic/Stripped]: ").strip()
    return self.approach == 'Realistic' or self.approach == 'Stripped'

  def __gather_classification(self):
    print("""
  The classification of your test implementation.

  Fullstack: Robust framework expected to provide high-level functionality for
             serving as a web application; for example, ability to compose 
             views, provide functions for responding with several data types 
             (json, html, etc), connecting to a database, form processing, etc.
  Micro:     Simple framework expected to provide enough middleware to build a
             robust web application such as request routing and some simple 
             plumbing, but may not include built-in functionality such as, for 
             example, server-composed views.
  Platform:  Barebones infrastructure for servicing HTTP requests, but does 
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
    self.classification = raw_input("Classification [Fullstack/Micro/Platform]: ").strip()
    return self.classification == 'Fullstack' or \
           self.classification == 'Micro' or \
           self.classification == 'Platform'

  def __gather_platform(self):
    print("""
  The platform of your test implementation.

  The platform is the low-level software or API used to host web applications 
  for the framework; the platform provides an implementation of the HTTP
  fundamentals.

  Not all frameworks have a platform.
    """)
    self.__prompt_platform()
    
  def __prompt_platform(self):
    self.platform = raw_input("Platform (optional): ").strip()

  def __gather_orm(self):
    print("""
  How you would classify the ORM (object relational mapper) of your test?

  Full:  A feature-rich ORM which provides functionality for interacting with a
         database without writing a query in all but the most edge cases.
  Micro: An ORM which provides functionality for interacting with a database
         for many trivial operations (querying, updating), but not more robust
         cases (for example, gathering relations).
  Raw:   No ORM; raw database access.
    """)
    valid = self.__prompt_orm()
    while not valid:
      valid = self.__prompt_orm()

  def __prompt_orm(self):
    self.orm = raw_input("ORM [Full/Micro/Raw]: ").strip()
    return self.orm == 'Full' or \
           self.orm == 'Micro' or \
           self.orm == 'Raw'

  def __gather_webserver(self):
    print("""
  Name of the front-end web-server sitting in front of your test implementation.

  Your test implementation may not use a web-server and may act as its own; you
  can leave this blank in this case.

  Example: nginx
    """)
    self.__prompt_webserver()

  def __prompt_webserver(self):
    self.webserver = raw_input("Webserver (optional): ").strip()

  def __gather_versus(self):
    print("""
  The name of another test (elsewhere in this project) that is a subset of this 
  framework.
  This allows for the generation of the framework efficiency chart in the 
  results web site.
  For example, Compojure is compared to "servlet" since Compojure is built on the 
  Servlet platform.
    """)
    self.__prompt_versus()

  def __prompt_versus(self):
    self.versus = raw_input("Versus (optional): ").strip()

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
    """ % (self.display_name, 
           self.language, 
           self.approach, 
           self.classification, 
           self.platform,
           self.orm, 
           self.webserver, 
           self.versus))

    valid = self.__prompt_confirmation()
    while not valid:
      valid = self.__prompt_confirmation()

    if self.confirmation == 'y':
      self.__build_scaffolding()
    else:
      print('Aborting')

  def __prompt_confirmation(self):
    self.confirmation = raw_input("Initialize [y/n]: ")
    return self.confirmation == 'y' or self.confirmation == 'n'

  def __build_scaffolding(self):
    if self.__create_test_folder():
      self.__copy_scaffold_files()
      self.__edit_scaffold_files()

  def __create_test_folder(self):
    self.language_dir = os.path.join("frameworks", self.language)
    self.test_dir = os.path.join(self.language_dir, self.name)

    if os.path.exists(self.test_dir):
      print("Test '%s' already exists; aborting." % self.name)
      return False

    return True

  def __copy_scaffold_files(self):
    self.scaffold_dir = os.path.join("toolset","setup","scaffolding")
    copytree(self.scaffold_dir, self.test_dir)

  def __edit_scaffold_files(self):
    for file in os.listdir(os.path.join(self.test_dir)):
      replace_text(os.path.join(self.test_dir, file), "\$NAME", self.name)
      replace_text(os.path.join(self.test_dir, file), "\$DISPLAY_NAME", self.display_name)
      replace_text(os.path.join(self.test_dir, file), "\$APPROACH", self.approach)
      replace_text(os.path.join(self.test_dir, file), "\$CLASSIFICATION", self.classification)
      replace_text(os.path.join(self.test_dir, file), "\$FRAMEWORK", self.framework)
      replace_text(os.path.join(self.test_dir, file), "\$LANGUAGE", self.language)
      replace_text(os.path.join(self.test_dir, file), "\$ORM", self.orm)
      replace_text(os.path.join(self.test_dir, file), "\$PLATFORM", self.platform)
      replace_text(os.path.join(self.test_dir, file), "\$WEBSERVER", self.webserver)
      replace_text(os.path.join(self.test_dir, file), "\$VERSUS", self.versus)