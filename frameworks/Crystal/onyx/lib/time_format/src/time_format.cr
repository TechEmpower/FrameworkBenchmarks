# Available methods (for convenience):
#
# - `#auto`
# - `#minutes`
# - `#m`
# - `#seconds`
# - `#s`
# - `#milliseconds`
# - `#ms`
# - `#microseconds`
# - `#μs`
# - `#mcs` (alias of `#μs`)
module TimeFormat
  extend self

  # :nodoc:
  MINUTES_IN_SECOND = 1 / 60.0

  # :nodoc:
  MILLISECONDS_IN_SECOND = 1_000.0

  # :nodoc:
  MICROSECONDS_IN_SECOND = 1_000_000.0

  # Returns time in human readable format based on how big it is
  #
  # ```
  # TimeFormat.auto(1.minute, false)    # => 1 minute
  # TimeFormat.auto(1.milliseconds / 2) # => 500μs
  # ```
  def auto(time : Float | Time::Span, short = true) : String
    return (short ? m(time) : minutes(time)) if time.to_f / 60.0 >= 1
    return (short ? s(time) : seconds(time)) if time.to_f >= 1
    return (short ? ms(time) : milliseconds(time)) if time.to_f * 1000.0 >= 1
    return (short ? μs(time) : microseconds(time))
  end

  # Returns time in minutes in human readable format
  #
  # ```
  # TimeFormat.minutes(5.minutes + 36.seconds + 175.milliseconds) # => 5.6 minutes
  # ```
  def minutes(time : Float | Time::Span, round : Int32 = 2) : String
    m = (time.to_f * MINUTES_IN_SECOND).round(round)
    m == 1.0 ? "1 minute" : m.to_s + " minutes"
  end

  # Returns time in minutes in **short** human readable format
  #
  # ```
  # TimeFormat.m(5.minutes + 36.seconds + 175.milliseconds) # => 5.6m
  # ```
  def m(time, round = 2)
    (time.to_f * MINUTES_IN_SECOND).round(round).to_s + "m"
  end

  # Returns time in seconds in human readable format
  #
  # ```
  # TimeFormat.seconds(5.minutes + 36.seconds + 175.milliseconds) # => 336.175 seconds
  # ```
  def seconds(time, round = 3) : String
    s = (time.to_f).round(round)
    s == 1.0 ? "1 second" : s.to_s + " seconds"
  end

  # Returns *time* in seconds in **short** human readable format
  #
  # ```
  # TimeFormat.s(5.minutes + 36.seconds + 175.milliseconds) # => 336.175s
  # ```
  def s(time, round = 3)
    (time.to_f).round(round).to_s + "s"
  end

  # Returns *time* in milliseconds in human readable format
  #
  # ```
  # TimeFormat.milliseconds(5.minutes + 36.seconds + 175.milliseconds) # => 336175.0 milliseconds
  # ```
  def milliseconds(time : Float | Time::Span, round : Int32 = 3) : String
    ms = (time.to_f * MILLISECONDS_IN_SECOND).round(round)
    ms == 1.0 ? "1 millisecond" : ms.to_s + " milliseconds"
  end

  # Returns *time* in milliseconds in **short** human readable format
  #
  # ```
  # TimeFormat.ms(5.minutes + 36.seconds + 175.milliseconds) # => 336175.0ms
  # ```
  def ms(time : Float | Time::Span, round : Int32 = 3)
    (time.to_f * MILLISECONDS_IN_SECOND).round(round).to_s + "ms"
  end

  # Returns *time* in microseconds in human readable format
  #
  # ```
  # TimeFormat.microseconds(5.minutes + 36.seconds + 175.milliseconds) # => 336175000 microseconds
  # ```
  def microseconds(time : Float | Time::Span) : String
    μs = (time.to_f * MICROSECONDS_IN_SECOND).to_i
    μs == 1 ? "1 microsecond" : μs.to_s + " microseconds"
  end

  # Returns *time* in microseconds in **short** human readable format
  #
  # ```
  # TimeFormat.μs(5.minutes + 36.seconds + 175.milliseconds) # => 336175000μs
  # ```
  def μs(time : Float | Time::Span)
    (time.to_f * MICROSECONDS_IN_SECOND).to_i.to_s + "μs"
  end

  # ditto
  def mcs(time : Float | Time::Span)
    μs(time)
  end
end
