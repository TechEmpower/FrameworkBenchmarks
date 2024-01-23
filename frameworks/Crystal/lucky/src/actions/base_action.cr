abstract class BaseAction < Lucky::Action
  ID_MAXIMUM = 10_000

  include AddRequiredHeaders

  disable_cookies
  accepted_formats [:json, :html, :plain_text], default: :json
end
