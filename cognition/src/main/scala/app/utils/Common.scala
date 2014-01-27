package utils

object Common {

  @inline
  def normalizeInt(number: Int, start: Int, end: Int) =
    if (number < start)
      start
    else if (number > end)
      end
    else
      number

}
