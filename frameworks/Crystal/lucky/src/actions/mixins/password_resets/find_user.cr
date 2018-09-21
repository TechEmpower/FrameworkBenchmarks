module Auth::PasswordResets::FindUser
  private def user : User
    UserQuery.find(user_id)
  end
end
