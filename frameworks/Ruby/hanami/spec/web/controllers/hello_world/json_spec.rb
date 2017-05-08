require 'spec_helper'
require_relative '../../../../apps/web/controllers/hello_world/json'

describe Web::Controllers::HelloWorld::Json do
  let(:action) { Web::Controllers::HelloWorld::Json.new }
  let(:params) { Hash[] }

  it 'is successful' do
    response = action.call(params)
    response[0].must_equal 200
  end
end
