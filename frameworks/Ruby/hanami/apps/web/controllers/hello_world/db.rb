module Web::Controllers::HelloWorld
  class Db
    include Web::Action

    def call(params)
      self.headers.merge!({ 'Content-Type' => 'application/json' })
      status 200, WorldRepository.new.find_random_entity.to_h.to_json
    end
  end
end
