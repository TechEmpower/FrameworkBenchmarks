require "../spec_helper"

describe "Authentication flow" do
  it "works" do
    flow = AuthenticationFlow.new("test@example.com")

    flow.sign_up "password"
    flow.should_be_signed_in
    flow.sign_out
    flow.sign_in "wrong-password"
    flow.should_have_password_error
    flow.sign_in "password"
    flow.should_be_signed_in
  end

  # This is to show you how to sign in as a user during tests.
  # Use the `visit` method's `as` option in your tests to sign in as that user.
  #
  # Feel free to delete this once you have other tests using the 'as' option.
  it "allows sign in through backdoor when testing" do
    user = UserBox.create
    flow = BaseFlow.new

    flow.visit Me::Show, as: user
    should_be_signed_out(flow)
  end
end

private def should_be_signed_out(flow)
  flow.el("@sign-out-button").should be_on_page
end
