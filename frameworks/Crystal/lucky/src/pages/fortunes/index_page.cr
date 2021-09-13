class Fortunes::IndexPage
  include Lucky::HTMLPage

  needs fortunes : Array(Fortune)

  def render
    html_doctype

    html lang: "en" do
      head do
        title "Fortunes"
      end

      body do
        table do
          tr do
            th "id"
            th "message"
          end

          fortunes.each do |fortune|
            tr do
              td fortune.id
              # html text escaped by default in Lucky
              td fortune.message
            end
          end
        end
      end
    end
  end
end
