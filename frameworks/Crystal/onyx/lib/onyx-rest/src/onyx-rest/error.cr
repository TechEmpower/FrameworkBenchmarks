# A REST error which is expected to be rescued upon processing (i.e. by `Onyx::REST::Rescuers::REST`).
# Define your own errors to handle **expected** situations:
#
# ```
# class UserNotFound < Onyx::REST::Error(404)
#   def initialize(@id : Int32)
#     super("User not found with ID #{@id}")
#   end
#
#   def payload
#     {id: @id}
#   end
# end
#
# # Will return 404 erorr if a user isn't found by the ID
# router.get "/users/:id" do |env|
#   id = env.request.path_params["id"]?.to_i?
#   raise UserNotFound.new(id) unless Models::User.find?(id)
# end
# ```
#
# In case of error, a plain text response status and body will be
# `404` and `"404 User not found with ID 42"` respectively.
#
# `Onyx::REST::Renderers::JSON` takes care of the error's `#payload`, rendering it appropriately:
#
# ```json
# {
#   "error": {
#     "class": "UserNotFound",
#     "message": "User not found with ID 42",
#     "code": 404,
#     "payload": {
#       "id": 42
#     }
#   }
# }
# ```
class Onyx::REST::Error(Code) < Exception
  # The HTTP status code of this error.
  getter code : Int32 = Code

  # The name of the error. By default returns its class name.
  def name
    {{@type.name.split("::").last}}
  end

  # Initialize the error with default message, which is its class name splitted with space
  # (e.g. `"User Not Found"` for `UserNotFound` error).
  def initialize
    super({{@type.name.split("::").last.underscore.split('_').join(' ') { |s| s.capitalize }}})
  end

  # Initialize the error with a *message*.
  def initialize(message : String)
    super
  end

  # The error payload. Usually used by custom renderers, for example,
  # `Onyx::REST::Renderers::JSON` calls `error.payload.try &.to_json`.
  # Returns `nil` by default.
  def payload
  end
end
