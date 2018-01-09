class Scaffolding:
  def __init__(self):
    print """
    This wizard is intended to help build the scaffolding required for a new 
    test to be benchmarked.

    From here, you will be prompted for values related to the test you
    wish to add.

    You can enter '?' for any prompt to get a detailed explanation.
    """

    self.__gather_display_name()
    self.__gather_language()
    self.__gather_approach()
    self.__gather_classification()
    self.__gather_orm()
    self.__gather_webserver()
    self.__gather_versus()

    self.__confirm_values()

  def __gather_display_name(self):
    self.__prompt_display_name()
    while not self.display_name:
      self.__prompt_display_name()

  def __prompt_display_name(self):
    self.display_name = raw_input("Name: ").strip()
    if self.display_name == '?':
      print """
  The name of your test as you wish it to be displayed on the results page.
      """
      self.display_name = None

  def __gather_language(self):
    self.language = None
    while not self.language:
      self.__prompt_language()

  def __prompt_language(self):
    self.language = raw_input("Language: ").strip()
    if self.language == '?':
      print """
  The language in which your test implementation is written.

  Example: Java, Go, PHP
      """
      self.language = None
    return self.language

  def __gather_approach(self):
    valid = self.__prompt_approach()
    while not valid:
      valid = self.__prompt_approach()

  def __prompt_approach(self):
    self.approach = raw_input("Approach [Realistic/Stripped]: ").strip()
    if self.approach == '?':
      print """
  Approach has to do with the way your test is designed. If you create a 
  production-ready implementation, then your approach is 'Realistic'. If
  you create an implementation you would never expect to be used in a
  production setting (for one reason or another), then your approach is
  'Stripped'.
      """
    return self.approach == 'Realistic' or self.approach == 'Stripped'

  def __gather_classification(self):
    valid = self.__prompt_classification()
    while not valid:
      valid = self.__prompt_classification()
    if self.classification == 'Platform':
      self.platform = 'None'
      self.framework = 'None'
    else:
      self.framework = self.display_name

  def __prompt_classification(self):
    self.classification = raw_input("Classification [Fullstack/Micro/Platform]: ").strip()
    if self.classification == '?':
      print """
  How you would classify your test?

  Fullstack: Robust framework expected to provide high-level functionality for
             serving as a web application; for example, ability to compose views,
             provide functions for responding with several data types (json, html, 
             etc), connecting to a database, etc.
  Micro:     Simple framework expected to provide enough middleware to build a
             robust web application, but may not include built-in functionality
             such as, for example, server-composed views.
  Platform:  Barebones infrastructure for servicing HTTP requests, but does not,
             for example, include built-in functionality for routing requests.
      """
      self.classification = None
    return self.classification == 'Fullstack' or \
           self.classification == 'Micro' or \
           self.classification == 'Platform'

  def __gather_orm(self):
    valid = self.__prompt_orm()
    while not valid:
      valid = self.__prompt_orm()

  def __prompt_orm(self):
    self.orm = raw_input("ORM [Full/Micro/Raw]: ").strip()
    if self.orm == '?':
      print """
  How you would classify the ORM (object relational mapper) of your test?

  Full:  A feature-rich ORM which provides functionality for interacting with a
         database without writing a query in all but the most edge cases.
  Micro: An ORM which provides functionality for interacting with a database
         for many trivial operations (querying, updating), but not more robust
         cases (for example, gathering relations).
  Raw:   No ORM; raw database access.
      """
      self.orm = None
    return self.orm == 'Full' or \
           self.orm == 'Micro' or \
           self.orm == 'Raw'

  def __gather_webserver(self):
    valid = self.__prompt_webserver()
    while not valid:
      valid = self.__prompt_webserver()

  def __prompt_webserver(self):
    self.webserver = raw_input("Webserver (optional): ").strip()
    if self.webserver == '?':
      print """
  Name of the front-end web-server sitting in front of your test implementation.

  Your test implementation may not use a web-server and may act as its own; you
  can leave this blank in this case.

  Example: nginx
      """
      self.webserver = None
      return False
    return True

  def __gather_versus(self):
    valid = self.__prompt_versus()
    while not valid:
      valid = self.__prompt_versus()

  def __prompt_versus(self):
    self.versus = raw_input("Versus (optional): ").strip()
    if self.versus == '?':
      print """
  The name of another test (elsewhere in this project) that is a subset of this 
  framework.
  This allows for the generation of the framework efficiency chart in the 
  results web site.
  For example, Compojure is compared to "servlet" since Compojure is built on the 
  Servlet platform.
      """
      self.versus = None
      return False
    return True

  def __confirm_values(self):
    print """
    Name: %s
    Language: %s
    Approach: %s
    Classification: %s
    ORM: %s
    Webserver: %s
    Versus: %s
    """ % (self.display_name, 
           self.language, 
           self.approach, 
           self.classification, 
           self.orm, 
           self.webserver, 
           self.versus)

    valid = self.__prompt_confirmation()
    while not valid:
      valid = self.__prompt_confirmation()

    if self.confirmation == 'y':
      self.__build_scaffolding()
    else:
      print 'Aborting'

  def __prompt_confirmation(self):
    self.confirmation = raw_input("Initialize [y/n]: ")
    if self.confirmation == '?':
      print """
  Finalize the initialization of your test given the above values?

  Note: once you have initialized your test, you can change these values later.
      """
      return False
    return self.confirmation == 'y' or self.confirmation == 'n'

  def __build_scaffolding(self):
    # Do some work here