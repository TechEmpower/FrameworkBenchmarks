require 'spec_helper'
require_relative '../../../../apps/web/controllers/hello_world/fortune'

describe Web::Controllers::HelloWorld::Fortune do
  let(:action) { Web::Controllers::HelloWorld::Fortune.new }
  let(:params) { Hash[] }

  it 'is successful' do
    response = action.call(params)
    response[0].must_equal 200
  end
end
