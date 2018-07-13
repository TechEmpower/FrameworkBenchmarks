abstract class BaseEmail < Carbon::Email
  # You can add defaults using the 'inherited' hook
  #
  # Example:
  #
  #   macro inherited
  #     from default_from
  #   end
  #
  #   def default_from
  #     Carbon::Address.new("support@app.com")
  #   end
end
