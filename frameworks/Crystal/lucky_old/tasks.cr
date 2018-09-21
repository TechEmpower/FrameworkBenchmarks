# Load Lucky and the app (actions, models, etc.)
require "./src/app"

# You can add your own tasks here in the ./tasks folder
require "./tasks/**"

# Load migrations
require "./db/migrations/**"

# Load Lucky tasks (dev, routes, etc.)
require "lucky/tasks/**"

LuckyCli::Runner.run
