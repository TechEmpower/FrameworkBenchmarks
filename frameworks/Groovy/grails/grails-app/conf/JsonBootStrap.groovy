import org.codehaus.groovy.grails.web.converters.Converter
import org.codehaus.groovy.grails.web.json.JSONWriter
import grails.converters.JSON
import hello.World

@groovy.transform.CompileStatic
class JsonBootStrap {
    def init = { servletContext ->
        JSON.registerObjectMarshaller(World, { World world, Converter converter ->
            JSONWriter writer = (JSONWriter)converter.writer
            writer.object()
            writer.key('id').value(world.id)
            writer.key('randomNumber').value(world.randomNumber)
            writer.endObject()
            null
        })
    }
}
