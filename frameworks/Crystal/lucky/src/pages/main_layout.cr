abstract class MainLayout
  # Edit shared layout code in src/components/shared/layout.cr
  include Shared::Layout

  # 'needs current_user : User' makes it so that the current_user
  # is always required for pages using MainLayout
  needs current_user : User

  def render
    html_doctype

    html lang: "en" do
      shared_layout_head

      body do
        render_flash
        render_signed_in_user
        content
      end
    end
  end

  private def render_signed_in_user
    text @current_user.email
    text " - "
    link "Sign out", to: SignIns::Delete, flow_id: "sign-out-button"
  end
end
