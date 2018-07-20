class Errors::Show < Lucky::ErrorAction
  def handle_error(error : JSON::ParseException)
    message = "There was a problem parsing the JSON." +
              " Please check that it is formed correctly"

    json Errors::ShowSerializer.new(message), status: 400
  end

  def handle_error(error : Lucky::RouteNotFoundError)
    json Errors::ShowSerializer.new("Not found"), status: 404
  end

  def handle_error(error : Exception)
    error.inspect_with_backtrace(STDERR)

    message = "An unexpected error occurred"

    json Errors::ShowSerializer.new(message), status: 500
  end
end
