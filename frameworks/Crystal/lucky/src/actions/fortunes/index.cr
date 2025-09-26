class Fortunes::Index < BaseAction
  get "/fortunes" do
    fortunes = Fortune::BaseQuery.all.results
    fortunes << Fortune.new(id: 0, message: "Additional fortune added at request time.")
    fortunes.sort_by!(&.message)
    html Fortunes::IndexPage, fortunes: fortunes
  end
end
