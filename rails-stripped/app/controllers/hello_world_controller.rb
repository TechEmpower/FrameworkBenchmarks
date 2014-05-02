class HelloWorldController < ActionController::Metal
  include ActionController::Rendering
  include ActionController::Renderers::All

  def json
    render json: { message: "Hello, World!" }
  end

  def db
    queries = (params[:queries] || 1).to_i

    if queries > 1
      results = (1..queries).map do
        # get a random row from the database, which we know has 10000
        # rows with ids 1 - 10000
        World.find(Random.rand(10000) + 1)
      end
    else
      results = World.find(Random.rand(10000) + 1)
    end
    
    self.content_type = "text/json"
    render json: results
  end
  
  def render(opts = {})
    self.content_type = "text/json"
    self.response_body = opts[:json].to_json
  end
end
