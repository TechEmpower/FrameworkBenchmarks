module AddRequiredHeaders
  macro included
    after add_required_headers
  end

  def add_required_headers
    response.headers["Server"] = "Lucky"
    continue
  end
end
