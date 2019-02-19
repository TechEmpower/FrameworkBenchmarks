module Onyx::SQL
  class Repository
    # Call `db.exec(sql, params)`.
    def exec(sql : String, params : Enumerable(DB::Any)? = nil) : DB::ExecResult
      sql = prepare_query(sql)

      @logger.wrap("[#{driver}] #{sql}") do
        if params
          db.exec(sql, params.to_a)
        else
          db.exec(sql)
        end
      end
    end

    # Call `db.exec(sql, *params)`.
    def exec(sql : String, *params : DB::Any) : DB::ExecResult
      exec(sql, params)
    end

    # Call `db.exec(*query.build)`. It removes any `Query#returning` clauses to avoid DB hanging.
    def exec(query : Query) : DB::ExecResult
      raise ArgumentError.new("Must not call 'Repository#exec' with SELECT Query. Consider using 'Repository#scalar' or 'Repository#query' instead") if query.type.select?

      # Removes `RETURNING` clause, so the DB doesn't hang! 🍬
      query.returning = nil

      exec(*query.build(postgresql?))
    end
  end
end
