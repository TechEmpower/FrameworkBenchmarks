module Onyx::SQL
  class Query(T)
    # The `ORDER BY` clause order.
    enum Order
      Desc
      Asc
    end

    # Add `ORDER BY` clause either by explicit *value* or by a `T` field.
    # When *value* is a `Symbol`, it's treated as a `T` field and tried to be found in
    # **runtime**, raising otherwise. So you'll not know if you mistyped until the code is run.
    #
    # NOTE: You must refer a `T` field by its instance variable name, not the DB column name.
    #
    # ```
    # q = User.all.order_by(:id, :desc)
    # q.build # => {"SELECT users.* FROM users ORDER BY id DESC"}
    #
    # q = User.all.order_by("foo(bar)")
    # q.build # => {"SELECT users.* FROM users ORDER BY foo(bar)"}
    # ```
    #
    # TODO: Make it type-safe.
    def order_by(value : Symbol | String, order : Order? = nil)
      {% begin %}
        case value
        {% for ivar in T.instance_vars %}
          when {{ivar.name.symbolize}}
            ensure_order_by.add(OrderBy.new(
              "#{@alias || {{T.annotation(Model::Options)[:table].id.stringify}}}.#{T.db_column({{ivar.name.symbolize}})}",
              order
            ))
        {% end %}
        when Symbol
          raise ":#{value} symbol didn't match any of #{T} instance variables"
        else
          ensure_order_by.add(OrderBy.new(value, order))
        end
      {% end %}

      self
    end

    private struct OrderBy
      getter column, order

      def initialize(@column : String, @order : Order? = nil)
      end
    end

    @order_by : ::Set(OrderBy)? = nil

    protected def get_order_by
      @order_by
    end

    protected def ensure_order_by
      @order_by ||= ::Set(OrderBy).new
    end

    protected def append_order_by(sql, *args)
      return if @order_by.nil? || ensure_order_by.empty?

      sql << " ORDER BY "

      first = true
      ensure_order_by.each do |order_by|
        sql << ", " unless first; first = false
        sql << order_by.column

        if order = order_by.order
          sql << " " << order.to_s.upcase
        end
      end
    end
  end
end
