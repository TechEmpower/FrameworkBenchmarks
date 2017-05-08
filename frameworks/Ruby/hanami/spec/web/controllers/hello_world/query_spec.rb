require 'spec_helper'
require_relative '../../../../apps/web/controllers/hello_world/query'

describe Web::Controllers::HelloWorld::Query do
  let(:action) { Web::Controllers::HelloWorld::Query.new }
  let(:params) { Hash[] }

  it 'is successful' do
    response = action.call(params)
    response[0].must_equal 200
  end
end
