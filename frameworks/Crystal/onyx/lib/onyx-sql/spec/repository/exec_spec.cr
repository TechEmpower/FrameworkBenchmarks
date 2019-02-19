require "../repository_spec"

describe Repository do
  db = MockDB.new
  repo = Repository.new(db)

  describe "#exec" do
    context "with paramsless Query" do
      result = repo.exec(Query(User).new.update.set("foo = 42"))

      it "calls DB#exec with valid sql" do
        db.latest_exec_sql.should eq <<-SQL
        UPDATE users SET foo = 42
        SQL
      end

      it "does not pass any params to DB#exec" do
        db.latest_exec_params.not_nil!.should be_empty
      end
    end

    context "with params Query" do
      result = repo.exec(Query(User).new.update.set(active: true))

      it "calls DB#exec with valid sql" do
        db.latest_exec_sql.should eq <<-SQL
        UPDATE users SET activity_status = ?
        SQL
      end

      it "pass params to DB#exec" do
        db.latest_exec_params.should eq [true]
      end
    end
  end
end
