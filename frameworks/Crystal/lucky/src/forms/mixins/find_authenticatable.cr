module FindAuthenticatable
  private def find_authenticatable
    email.value.try do |value|
      UserQuery.new.email(value).first?
    end
  end
end
