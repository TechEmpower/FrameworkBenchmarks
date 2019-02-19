require "../query_spec"

describe "Query#having" do
  context "without params" do
    it do
      q = Query(User).new.having("foo")

      sql, params = q.build
      sql.should eq <<-SQL
      SELECT users.* FROM users HAVING (foo)
      SQL

      params.should be_empty
    end
  end

  context "with params" do
    it do
      q = Query(User).new.having("foo = ? AND bar = ?", 42, 43).having("foo")

      sql, params = q.build
      sql.should eq <<-SQL
      SELECT users.* FROM users HAVING (foo = ? AND bar = ?) AND (foo)
      SQL

      params.to_a.should eq [42, 43]
    end
  end

  describe "shorthands" do
    describe "#having_not" do
      context "without params" do
        it do
          Query(User).new.having_not("foo = 'bar'").to_s.should eq <<-SQL
          SELECT users.* FROM users HAVING NOT (foo = 'bar')
          SQL
        end
      end

      context "with params" do
        it do
          q = Query(User).new.having_not("foo = ?", 42)

          sql, params = q.build
          sql.should eq <<-SQL
          SELECT users.* FROM users HAVING NOT (foo = ?)
          SQL

          params.to_a.should eq [42]
        end
      end
    end

    describe "manually tested" do
      uuid = UUID.random

      it do
        q = Query(User).new.having("activity_status IS NOT NULL").and_not("name = ?", "John").or_having("foo")

        sql, params = q.build
        sql.should eq <<-SQL
        SELECT users.* FROM users HAVING (activity_status IS NOT NULL) AND NOT (name = ?) OR (foo)
        SQL

        params.to_a.should eq ["John"]
      end
    end

    # It has almost zero benefit for you as a reader, but it allows to check that all methods delegate their arguments as expected.
    #
    # Methods which are tested:
    #
    # - `#or_having_not`
    # - `#or_having`
    # - `#and_having_not`
    # - `#and_having`
    #
    # Each method has two variants (clause with params, single clause) and two situations - when it's called for first time (e.g. `Query.new.and_having`) and when it's called afterwards (e.g. `Query.new.having.and_having`), which results in 16 tests. I decided that it would be simpler to use macros, which however require some skill to understand.
    {% for or in [true, false] %}
      {% for not in [true, false] %}
        describe '#' + {{(or ? "or" : "and")}} + "_having" do
          context "when first call" do
            context "without params" do
              it do
                Query(User).new.{{(or ? "or" : "and").id}}_having{{"_not".id if not}}("foo = 'bar'").to_s.should eq <<-SQL
                SELECT users.* FROM users HAVING {{"NOT ".id if not}}(foo = 'bar')
                SQL
              end
            end

            context "with params" do
              it do
                q = Query(User).new.{{(or ? "or" : "and").id}}_having{{"_not".id if not}}("foo = ?", 42)

                sql, params = q.build
                sql.should eq <<-SQL
                SELECT users.* FROM users HAVING {{"NOT ".id if not}}(foo = ?)
                SQL

                params.to_a.should eq [42]
              end
            end
          end

          context "when non-first call" do
            context "without params" do
              it do
                Query(User).new.having("first = true").{{(or ? "or" : "and").id}}_having{{"_not".id if not}}("foo = 'bar'").to_s.should eq <<-SQL
                SELECT users.* FROM users HAVING (first = true) {{or ? "OR ".id : "AND ".id}}{{"NOT ".id if not}}(foo = 'bar')
                SQL
              end
            end

            context "with params" do
              it do
                q = Query(User).new.having("first = true").{{(or ? "or" : "and").id}}_having{{"_not".id if not}}("foo = ?", 42)

                sql, params = q.build
                sql.should eq <<-SQL
                SELECT users.* FROM users HAVING (first = true) {{or ? "OR ".id : "AND ".id}}{{"NOT ".id if not}}(foo = ?)
                SQL

                params.to_a.should eq [42]
              end
            end
          end
        end
      {% end %}
    {% end %}
  end
end
