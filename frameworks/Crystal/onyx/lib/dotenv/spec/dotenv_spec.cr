require "./spec_helper"

Dotenv.verbose = false

describe Dotenv do
  context "Non exsisting file." do
    it "should print warning" do
      Dotenv.load ".some-non-existent-env-file"
    end
  end

  context "Exsisting file" do
    File.write ".test-env", "VAR=Hello"

    it "should load env" do
      Dotenv.load ".test-env"
      ENV["VAR"].should eq "Hello"
    end

    it "should return hash" do
      hash = Dotenv.load ".test-env"
      hash["VAR"].should eq "Hello"
    end

    File.delete ".test-env"
  end

  context "Comment Lines and Empty Lines" do
    File.write ".test-env", "# This is a comment\nVAR=Dude\n\n"

    it "should ignore" do
      hash = Dotenv.load ".test-env"
      hash.should eq({"VAR" => "Dude"})
    end

    File.delete ".test-env"
  end

  context "Values with special characters" do
    File.write ".test-env", "VAR=postgres://foo@localhost:5432/bar?max_pool_size=10"

    it "should allow `=` in values" do
      hash = Dotenv.load ".test-env"
      hash.should eq({"VAR" => "postgres://foo@localhost:5432/bar?max_pool_size=10"})
    end

    File.delete ".test-env"
  end

  context "From IO" do
    it "should load env" do
      io = IO::Memory.new "VAR2=test\nVAR3=other"
      hash = Dotenv.load io
      hash["VAR2"].should eq "test"
      hash["VAR3"].should eq "other"
      ENV["VAR2"].should eq "test"
      ENV["VAR3"].should eq "other"
    end
  end

  context "From Hash" do
    it "should load env" do
      hash = Dotenv.load({"test" => "test"})
      hash["test"].should eq "test"
      ENV["test"].should eq "test"
    end
  end

  context "Invalid file" do
    File.write ".test-env", "VAR1=Hello\nHELLO:asd"

    it "should read valid lines only" do
      Dotenv.load ".test-env"
      ENV["VAR1"].should eq "Hello"

      expect_raises do
        ENV["HELLO"]
      end
    end

    File.delete ".test-env"
  end

  describe "#load!" do
    context "Missing file" do
      it "should raise FileMissing error" do
        expect_raises(Dotenv::FileMissing) do
          Dotenv.load! ".test-env"
        end
      end
    end

    context "Exsisting file" do
      File.write ".test-env", "VAR=Hello"

      it "should load env" do
        Dotenv.load! ".test-env"
        ENV["VAR"].should eq "Hello"
      end

      it "should return hash" do
        hash = Dotenv.load! ".test-env"
        hash["VAR"].should eq "Hello"
      end

      File.delete ".test-env"
    end

    context "From IO" do
      it "should load env" do
        io = IO::Memory.new "VAR2=test\nVAR3=other"
        hash = Dotenv.load! io
        hash["VAR2"].should eq "test"
        hash["VAR3"].should eq "other"
        ENV["VAR2"].should eq "test"
        ENV["VAR3"].should eq "other"
      end
    end

    context "From Hash" do
      it "should load env" do
        hash = Dotenv.load!({"test" => "test"})
        hash["test"].should eq "test"
        ENV["test"].should eq "test"
      end
    end
  end
end
