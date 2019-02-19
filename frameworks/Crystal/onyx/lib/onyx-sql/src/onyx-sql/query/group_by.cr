module Onyx::SQL
  class Query(T)
    # Add `GROUP_BY` clause.
    def group_by(values : Enumerable(String))
      values.each do |value|
        ensure_group_by << value
      end

      self
    end

    # ditto
    def group_by(*values : String)
      group_by(values)
    end

    @group_by : Deque(String)? = nil

    protected def get_group_by
      @group_by
    end

    protected def ensure_group_by
      @group_by ||= Deque(String).new
    end

    protected def append_group_by(sql, *args)
      return if @group_by.nil? || ensure_group_by.empty?

      sql << " GROUP BY "

      first = true
      ensure_group_by.each do |value|
        sql << ", " unless first; first = false
        sql << value
      end
    end
  end
end
