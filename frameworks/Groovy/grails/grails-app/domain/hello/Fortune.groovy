package hello

import grails.compiler.GrailsCompileStatic

@GrailsCompileStatic
class Fortune {
    Integer id
    String message

    static constraints = {
    }

    static mapping = {
      table name: 'Fortune'
      version false
    }
}
