class Fortunes::IndexPage < MainLayout
  needs fortunes : Array(Fortune)

  def content
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
