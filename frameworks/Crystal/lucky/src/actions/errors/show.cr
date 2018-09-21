class Errors::Show < Lucky::ErrorAction
  def handle_error(error : JSON::ParseException)
    message = "There was a problem parsing the JSON." +
              " Please check that it is formed correctly"

    if json?
      json Errors::ShowSerializer.new(message), status: 400
    else
      render_error_page status: 500
    end
  end

  def handle_error(error : Lucky::RouteNotFoundError)
    if json?
      json Errors::ShowSerializer.new("Not found"), status: 404
    else
      render_error_page title: "Sorry, we couldn't find that page.", status: 404
    end
  end

  def handle_error(error : Exception)
    error.inspect_with_backtrace(STDERR)

    message = "An unexpected error occurred"

    if json?
      json Errors::ShowSerializer.new(message), status: 500
    else
      render_error_page status: 500
    end
  end

  private def render_error_page(status : Int32, title : String = "We're sorry. Something went wrong.")
    context.response.status_code = status
    render Errors::ShowPage, status: status, title: title
  end
end
