require "onyx-http/router"
require "../ext/http/server/response/view"

class Onyx::HTTP::Router
  # Draw a route for *path* and *methods* calling *action*. See `Action`.
  #
  # ```
  # router = Onyx::REST::Router.new do
  #   on "/foo", methods: %w(get post), MyAction
  # end
  # ```
  def on(path, methods : Array(String), action : REST::Action.class)
    methods.map(&.downcase).each do |method|
      add("/" + method + path, ContextProc.new do |context|
        view? = action.call(context)

        if view = view?.as?(REST::View)
          context.response.view ||= view
        end
      end.as(Node))
    end
  end

  {% for method in HTTP_METHODS %}
    # Draw a route for *path* with `{{method.upcase.id}}` calling *action*. See `Action`.
    #
    # ```
    # router = Onyx::REST::Router.new do
    #   {{method.id}} "/bar", MyAction
    # end
    # ```
    def {{method.id}}(path, action : REST::Action.class)
      on(path, [{{method}}], action)
    end
  {% end %}
end
