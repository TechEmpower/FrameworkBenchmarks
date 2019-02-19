require "../repository_spec"

describe Repository do
  db = MockDB.new
  repo = Repository.new(db)

  describe "#scalar" do
    context "with paramsless Query" do
      result = repo.scalar(Query(User).new.update.set("foo = 42").returning(:uuid))

      it "calls DB#scalar with valid sql" do
        db.latest_scalar_sql.should eq <<-SQL
        UPDATE users SET foo = 42 RETURNING users.uuid
        SQL
      end

      it "does not pass any params to DB#scalar" do
        db.latest_scalar_params.not_nil!.should be_empty
      end
    end

    context "with params Query" do
      result = repo.scalar(Query(User).new.update.set(active: true).returning(:active))

      it "calls DB#scalar with valid sql" do
        db.latest_scalar_sql.should eq <<-SQL
        UPDATE users SET activity_status = ? RETURNING users.activity_status
        SQL
      end

      it "pass params to DB#scalar" do
        db.latest_scalar_params.should eq [true]
      end
    end
  end
end
