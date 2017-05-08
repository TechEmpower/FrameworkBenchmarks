require 'spec_helper'
require_relative '../../../../apps/web/controllers/hello_world/plaintext'

describe Web::Controllers::HelloWorld::Plaintext do
  let(:action) { Web::Controllers::HelloWorld::Plaintext.new }
  let(:params) { Hash[] }

  it 'is successful' do
    response = action.call(params)
    response[0].must_equal 200
  end
end
