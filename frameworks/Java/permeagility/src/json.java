
import java.util.HashMap;
import permeagility.plus.json.JSONObject;
import permeagility.util.DatabaseConnection;
import permeagility.web.Download;

/** Test type 1: JSON serialization
 */
public final class json extends Download {

    @Override public String getContentType() { return "application/json"; }
    
    @Override public String getContentDisposition() { return null; }
    
    @Override public byte[] getBytes(DatabaseConnection con, HashMap<String, String> parms) {
        JSONObject jsonObject = new JSONObject().put("message", "Hello, World!");
	return jsonObject.toString().getBytes();
    }

}
