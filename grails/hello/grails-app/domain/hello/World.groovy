package hello

class World {
    Integer randomNumber

    static constraints = {
    }

    static mapping = {
      table name: 'World'
      version false
      columns {
        randomNumber     column:"randomNumber"
      }
    }
}
