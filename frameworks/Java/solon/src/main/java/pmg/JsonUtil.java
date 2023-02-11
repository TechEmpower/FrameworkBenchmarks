package pmg;

import io.edap.x.json.Eson;
import io.edap.x.json.JsonWriter;

import java.io.IOException;

import org.noear.solon.boot.web.MimeType;
import org.noear.solon.core.handle.Context;

/**
 * @author pmg1991
 * @version V1.0 
 */
public class JsonUtil {
	 public static void writeJsonBytes(Context c , Object obj) {
	        JsonWriter writer = Eson.THREAD_WRITER.get();
	        try {
	            writer.reset();
	            Eson.serialize(obj, writer);
	            //c.contentLength(writer.size());
	            c.contentType(MimeType.APPLICATION_JSON_VALUE);
	            writer.toStream(c.outputStream());
	        } catch (IOException e) {
	            throw new RuntimeException(e);
	        }
	    }
	
}
