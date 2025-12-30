# frozen_string_literal: true

require "hanami"

module HelloWorld
  class App < Hanami::App
    environment(:production) do
      config.logger.level = :error
    end
  end
end
