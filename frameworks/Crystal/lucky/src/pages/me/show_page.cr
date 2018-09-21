class Me::ShowPage < MainLayout
  def content
    h1 "This is your profile"
    h3 "Email:  #{@current_user.email}"
    helpful_tips
  end

  private def helpful_tips
    h3 "Next, you may want to:"
    ul do
      li "Modify this page: src/pages/me/show_page.cr"
      li "Change what route you go to after sign in: src/actions/home/index.cr"
      li "To add pages that do not require sign in, include the" +
         "Auth::SkipRequireSignIn module in your actions"
    end
  end
end
