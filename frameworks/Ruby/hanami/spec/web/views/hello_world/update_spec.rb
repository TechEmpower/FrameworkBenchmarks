require 'spec_helper'
require_relative '../../../../apps/web/views/hello_world/update'

describe Web::Views::HelloWorld::Update do
  let(:exposures) { Hash[foo: 'bar'] }
  let(:template)  { Hanami::View::Template.new('apps/web/templates/hello_world/update.html.erb') }
  let(:view)      { Web::Views::HelloWorld::Update.new(template, exposures) }
  let(:rendered)  { view.render }

  it 'exposes #foo' do
    skip 'This is an auto-generated test. Edit it and add your own tests.'

    # Example
    view.foo.must_equal exposures.fetch(:foo)
  end
end
