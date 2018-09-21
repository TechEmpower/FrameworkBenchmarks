abstract class GuestLayout
  # Edit shared layout code in src/components/shared/layout.cr
  include Shared::Layout

  def render
    html_doctype

    html lang: "en" do
      shared_layout_head

      body do
        render_flash
        content
      end
    end
  end
end
