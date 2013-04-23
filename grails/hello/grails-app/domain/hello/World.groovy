package hello

class World {
    Integer randomNumber

    static constraints = {
    }

    static mapping = {
      version false
      columns {
        randomNumber     column:"randomNumber"
      }
    }
}
