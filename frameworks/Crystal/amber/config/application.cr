require "amber"

require "./initializers/*"
require "../src/models/*"
require "../src/controllers/*"

Amber::Server.configure do |settings|
  # Use your environment variables settings here.
  #
  # Name: A name that identifies this application. This is not internally
  # used by the framework.
  #
  settings.name = "amber"
  #
  #
  # Host: is the application server host address or ip address. Useful for when
  # deploying Amber to a PAAS and likely the assigned server IP is either
  # known or unknown. Defaults to an environment variable HOST
  #
  settings.host = "0.0.0.0"
  #
  #
  # Port Reuse: Amber supports clustering mode which allows to spin
  # multiple app instances per core. This setting allows to bind the different
  # instances to the same port. Default this setting to true if the number or process
  # is grater than 1.
  #
  # > Read more about Linux PORT REUSE https://lwn.net/Articles/542629/
  #
  settings.port_reuse = true
  #
  #
  # Process Count: This will enable Amber to be used in cluster mode,
  # spinning an instance for each number of process specified here.
  # Rule of thumb, always leave at least 1 core available for system processes/resources.
  #
  settings.process_count = 1
  #
  #
  # PORT: This is the port that you're application will run on. Examples would be (80, 443, 3000, 8080)
  #
  settings.port = 8080
  #
  #
  # Log: Is the logger that will be used for Amber and it defaults to ::Logger.new(STDOUT).
  # You can supply a custom logger.
  #
  settings.logger = Amber::Environment::Logger.new(nil)
  #
  #
end
