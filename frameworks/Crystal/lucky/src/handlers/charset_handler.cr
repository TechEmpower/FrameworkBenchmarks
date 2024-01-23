class CharsetHandler
  include HTTP::Handler

  def call(context : HTTP::Server::Context)
    # Go to the next handler in the stack
    call_next(context)

    if html_content_type?(context.response)
      context.response.content_type = "text/html; charset=utf-8"
    end
  end

  private def html_content_type?(response)
    response.headers["Content-Type"]? == "text/html"
  end
end
