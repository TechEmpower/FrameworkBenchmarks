package pronghorn.types

class Fortune(val id: Int,
              val message: String): Comparable<Fortune> {
    override fun compareTo(other: Fortune): Int = message.compareTo(other.message)
}
