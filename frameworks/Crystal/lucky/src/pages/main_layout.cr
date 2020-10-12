abstract class MainLayout
  include Lucky::HTMLPage

  abstract def content

  def render
    html_doctype

    html lang: "en" do
      head do
        title "Fortunes"
      end

      body do
        content
      end
    end
  end
end
