# This module allows to define callbacks.
#
# Example usage:
#
# ```
# require "callbacks"
#
# class MyClass
#   include Callbacks
#
#   def call
#     with_callbacks do
#       puts "call"
#     end
#   end
#
#   before do
#     puts "before"
#   end
#
#   before do
#     puts "another before"
#   end
#
#   after do
#     puts "after"
#   end
#
#   after do
#     puts "another after"
#   end
# end
#
# MyClass.new.call
#
# # => before
# # => another before
# # => call
# # => after
# # => another after
# ```
#
# Objects including `Callbacks` can be inherited, please refer to each method's description for more information.
module Callbacks
  macro included
    {% unless @type.has_method?(:before) %}
      def before
      end
    {% end %}

    {% unless @type.has_method?(:after) %}
      def after
      end
    {% end %}
  end

  # Add before callback.
  #
  # Further before callbacks are called later in the scope of a single object. When defined in children, their before callbacks have higher precedence:
  #
  # ```
  # class Foo
  #   include Callbacks
  #
  #   before do
  #     puts "1"
  #   end
  #
  #   before do
  #     puts "2"
  #   end
  # end
  #
  # class Bar < Foo
  #   before do
  #     puts "3"
  #   end
  # end
  #
  # Bar.new.with_callbacks { puts "call" }
  # # => 3, 1, 2, call
  # ```
  macro before(&block)
    def before
      {% if existing = @type.methods.find { |d| d.name == "before" } %}
        {% if @type.superclass && @type.superclass.has_method?("before") %}
          {% to_insert = "#{yield}\nsuper" %}
          {{existing.body.stringify.gsub(/super/, to_insert).id}}
        {% else %}
          previous_def
          {{yield.id}}
        {% end %}
      {% else %}
        {% if @type.superclass && @type.superclass.has_method?(:before) %}
          ({{yield.id}})
          super
        {% else %}
          {{yield.id}}
        {% end %}
      {% end %}
    end
  end

  # Add after callback.
  #
  # Further after callbacks are called later in the scope of a single object. When defined in children, their after callbacks have lower precedence:
  #
  # ```
  # class Foo
  #   include Callbacks
  #
  #   after do
  #     puts "1"
  #   end
  #
  #   after do
  #     puts "2"
  #   end
  # end
  #
  # class Bar < Foo
  #   after do
  #     puts "3"
  #   end
  # end
  #
  # Bar.new.with_callbacks { puts "call" }
  # # => call, 1, 2, 3
  # ```
  macro after(&block)
    def after
      {% if @type.methods.find(&.name.== "after") %}
        previous_def
        {{yield.id}}
      {% elsif @type.superclass && @type.superclass.has_method?(:after) %}
        super
        {{yield.id}}
      {% else %}
        {{yield.id}}
      {% end %}
    end
  end

  def with_callbacks(&block)
    before
    result = yield
    after

    result
  end
end
