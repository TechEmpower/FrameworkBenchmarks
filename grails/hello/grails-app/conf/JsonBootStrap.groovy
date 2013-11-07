import org.codehaus.groovy.grails.web.converters.Converter;

import grails.converters.JSON
import hello.World


class JsonBootStrap {
    def init = { servletContext ->
        JSON.registerObjectMarshaller(World, { World world, Converter converter ->
            [id: world.id, randomNumber: world.randomNumber]
        })
    }
}
