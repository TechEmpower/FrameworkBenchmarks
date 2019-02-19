module Onyx::SQL
  class Query(T)
    # Add `OFFSET` clause. `nil` argument cancels it.
    def offset(@offset : Int32?)
      self
    end

    @offset : Int32? = nil

    protected def get_offset
      @offset
    end

    protected def append_offset(sql, *args)
      return unless @offset
      sql << " OFFSET " << @offset
    end
  end
end
