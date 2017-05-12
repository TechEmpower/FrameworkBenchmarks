module Web::Controllers::HelloWorld
  class Plaintext
    include Web::Action

    def call(params)
      self.format = :txt
      status 200, 'Hello, World!'
    end
  end
end
