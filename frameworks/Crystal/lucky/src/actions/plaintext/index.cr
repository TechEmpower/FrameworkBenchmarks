class Plaintext::Index < BaseAction
  get "/plaintext" do
    plain_text "Hello, World!"
  end
end
