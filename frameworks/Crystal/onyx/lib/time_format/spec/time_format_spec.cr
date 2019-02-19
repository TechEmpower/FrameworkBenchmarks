require "./spec_helper"

describe TimeFormat do
  span = 5.minutes + 36.seconds + 175.milliseconds + (1.0.milliseconds / 18)

  describe "#auto" do
    it "works" do
      TimeFormat.auto(span).should eq "5.6m"
    end

    it "works with short = false" do
      TimeFormat.auto(span, false).should eq "5.6 minutes"
    end
  end

  describe "#minutes" do
    it "works" do
      TimeFormat.minutes(span).should eq "5.6 minutes"
    end

    it "works when 1 minute" do
      TimeFormat.minutes(1.minute).should eq "1 minute"
    end
  end

  describe "#m" do
    it "works" do
      TimeFormat.m(span).should eq "5.6m"
    end
  end

  describe "#seconds" do
    it "works" do
      TimeFormat.seconds(span).should eq "336.175 seconds"
    end

    it "works when 1 second" do
      TimeFormat.seconds(1.second).should eq "1 second"
    end
  end

  describe "#s" do
    it "works" do
      TimeFormat.s(span).should eq "336.175s"
    end
  end

  describe "#milliseconds" do
    it "works" do
      TimeFormat.milliseconds(span).should eq "336175.056 milliseconds"
    end

    it "works when 1 millisecond" do
      TimeFormat.milliseconds(1.millisecond).should eq "1 millisecond"
    end
  end

  describe "#ms" do
    it "works" do
      TimeFormat.ms(span).should eq "336175.056ms"
    end
  end

  describe "#microseconds" do
    it "works" do
      TimeFormat.microseconds(span).should eq "336175055 microseconds"
    end

    it "works when 1 microsecond" do
      TimeFormat.microseconds(1.0.milliseconds / 1000).should eq "1 microsecond"
    end
  end

  describe "#μs" do
    it "works" do
      TimeFormat.μs(span).should eq "336175055μs"
    end
  end
end
