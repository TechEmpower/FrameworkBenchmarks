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
    render json: World.with_pk(random_id).values
  end

  def queries
    worlds = DB.synchronize do
      requested_ids.map do |id|
        World.with_pk(id)
      end
    end

    render json: worlds.map!(&:values)
  end

  def fortunes
    records = Fortune.all

    records << Fortune.new(id: 0, message: "Additional fortune added at request time.")
    records.sort_by!(&:message)

    render plain: FORTUNES_TEMPLATE.result(binding)
    headers["content-type"] = "text/html; charset=utf-8"
  end

  def updates
    worlds = nil

    DB.synchronize do
      worlds = requested_ids.map do |id|
        world = World.with_pk(id)
        new_value = random_id
        new_value = random_id while new_value == world.randomnumber
        world.randomnumber = new_value

        world
      end

      World.batch_update(worlds)
    end

    render json: worlds.map!(&:values)
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
