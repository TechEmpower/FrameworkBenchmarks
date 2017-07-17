module Web::Controllers::HelloWorld
  class Query
    include Web::Action

    def call(params)
      queries = params[:queries].to_i
      queries = 1 if queries < 1
      queries = 500 if queries > 500

      repository = WorldRepository.new
      results = (1..queries).map do
        repository.find_random_entity.to_h
      end
      status 200, results.to_json
    end
  end
end
