package hello

import grails.compiler.GrailsCompileStatic

@GrailsCompileStatic
class World {
    Integer id
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
