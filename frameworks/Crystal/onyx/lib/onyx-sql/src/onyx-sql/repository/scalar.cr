module Onyx::SQL
  class Repository
    # Call `db.scalar(sql, params)`.
    def scalar(sql : String, params : Enumerable(DB::Any)? = nil)
      sql = prepare_query(sql)

      @logger.wrap("[#{driver}] #{sql}") do
        if params
          db.scalar(sql, params.to_a)
        else
          db.scalar(sql)
        end
      end
    end

    # Call `db.scalar(sql, *params)`.
    def scalar(sql : String, *params : DB::Any)
      scalar(sql, params)
    end

    # Call `db.scalar(*query.build)`.
    def scalar(query : Query)
      scalar(*query.build(postgresql?))
    end
  end
end
