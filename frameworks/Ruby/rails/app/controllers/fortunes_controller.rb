# frozen_string_literal: true

class FortunesController < ApplicationController
  include DateHeader

  def index
    @fortunes = Fortune.all.to_a
    @fortunes << Fortune.new(id: 0, message: 'Additional fortune added at request time.')
    @fortunes.sort_by!(&:message)
    render :fortune
  end
end
