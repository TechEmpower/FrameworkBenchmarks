import org.codehaus.groovy.grails.web.json.JSONObject
import org.springframework.util.ClassUtils
import org.springframework.util.ReflectionUtils

class JsonWorkaroundBootStrap {
    def init = { servletContext ->
        // activate workaround for GRAILS-10823
        println("activating workaround for GRAILS-10823 - use this only for Grails 2.3.3")
        def encoderInstance = ClassUtils.forName("org.codehaus.groovy.grails.plugins.codecs.JSONEncoder", JSONObject.class.getClassLoader()).newInstance()
        ['javascriptEncoderStateless', 'javascriptEncoder'].each { fieldName ->
            ReflectionUtils.findField(JSONObject, fieldName).with {
                accessible = true
                set(null, encoderInstance)
            }
        }
    }
    def destroy = {
    }
}
