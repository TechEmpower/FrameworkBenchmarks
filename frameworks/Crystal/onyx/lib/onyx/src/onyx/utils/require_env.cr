# Define a required runtime environment variable which would raise on missing.
macro runtime_env(*envs)
  {% for env in envs %}
    raise "Runtime environment variable {{env}} is not defined!" unless ENV.has_key?("{{env}}")
  {% end %}
end

# Define a required buildtime environment variable which would raise on missing.
macro buildtime_env(*env)
  {% for var in envs %}
    {% raise "Buildtime environment variable #{var} is not defined!" unless env("#{var}") %}
  {% end %}
end
