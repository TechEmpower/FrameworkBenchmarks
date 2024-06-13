require_relative "config/application"

run Rage.application
Rage.load_middlewares(self)
