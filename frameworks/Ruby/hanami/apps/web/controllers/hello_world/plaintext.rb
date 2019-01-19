module Web::Controllers::HelloWorld
  class Plaintext
    include Web::Action

    def call(params)
      self.format = :txt
      self.headers.merge!({ 'Content-Type' => 'text/plain' })
      status 200, 'Hello, World!'
    end
  end
end
