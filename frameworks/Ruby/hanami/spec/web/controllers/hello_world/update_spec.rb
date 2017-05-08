require 'spec_helper'
require_relative '../../../../apps/web/controllers/hello_world/update'

describe Web::Controllers::HelloWorld::Update do
  let(:action) { Web::Controllers::HelloWorld::Update.new }
  let(:params) { Hash[] }

  it 'is successful' do
    response = action.call(params)
    response[0].must_equal 200
  end
end
