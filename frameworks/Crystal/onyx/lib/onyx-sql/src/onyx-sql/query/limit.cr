module Onyx::SQL
  class Query(T)
    # Add `LIMIT` clause. `nil` argument cancels it.
    def limit(@limit : Int32?)
      self
    end

    @limit : Int32? = nil

    protected def get_limit
      @limit
    end

    protected def append_limit(sql, *args)
      return unless @limit
      sql << " LIMIT " << @limit
    end
  end
end
