module Auth::SignInThroughBackdoor
  macro included
    before sign_in_through_backdoor
  end

  private def sign_in_through_backdoor
    if Lucky::Env.test? && (user_id = params.get?(:backdoor_user_id))
      user = UserQuery.find(user_id)
      sign_in user
    end
    continue
  end
end
