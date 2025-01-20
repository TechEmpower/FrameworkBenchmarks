# frozen_string_literal: true

class BenchmarksController < ApplicationController
  ALL_DB_IDS = (1..10_000).to_a
  FORTUNES_TEMPLATE = ERB.new(Rage.root.join("app/views/fortunes.html.erb").read)

  before_action do
    headers["server"] = "rage"
  end

  def json
    render json: { message: "Hello, World!" }
  end

  def plaintext
    render plain: "Hello, World!"
  end

  def db
    render json: World.find(random_id)
  end

  def queries
    records = requested_ids.map do |id|
      World.find(id)
    end

    render json: records
  end

  def fortunes
    records = Fortune.pluck(:id, :message).map! { |id, message| { id:, message: } }

    records << Fortune.new(id: 0, message: "Additional fortune added at request time.")
    records.sort_by! { |record| record[:message] }

    render plain: FORTUNES_TEMPLATE.result(binding)
    headers["content-type"] = "text/html; charset=utf-8"
  end

  def updates
    records = requested_ids.map do |id|
      World.find(id)
    end

    updates = records.map do |record|
      new_value = random_id
      new_value = random_id until new_value != record.randomNumber

      record.randomNumber = new_value

      { id: record.id, randomnumber: new_value }
    end

    World.upsert_all(updates.sort_by! { |u| u[:id] })

    render json: records
  end

  private

  def requested_ids
    num = params[:queries].to_i

    if num > 500
      num = 500
    elsif num < 1
      num = 1
    end

    ALL_DB_IDS.sample(num)
  end

  def random_id
    Random.rand(9_999) + 1
  end
end
