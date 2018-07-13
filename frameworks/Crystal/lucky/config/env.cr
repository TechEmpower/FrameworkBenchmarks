module Lucky::Env
  extend self

  {% for env in [:development, :test, :production] %}
    def {{ env.id }}?
      name == {{ env.id.stringify }}
    end
  {% end %}

  def name
    ENV["LUCKY_ENV"]? || "development"
  end
end
