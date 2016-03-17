
import com.orientechnologies.orient.core.record.impl.ODocument;
import java.util.HashMap;
import permeagility.plus.json.JSONArray;
import permeagility.plus.json.JSONObject;
import permeagility.util.DatabaseConnection;
import permeagility.web.Download;

/** Test type 3: Multiple Database Queries
 */
public final class queries extends Download {

    @Override public String getContentType() { return "application/json"; }
    
    @Override public String getContentDisposition() { return null; }
    
    @Override public byte[] getBytes(DatabaseConnection con, HashMap<String, String> parms) {
         String q = parms.get("queries");
        int qn = -1;
        try { 
            qn = Integer.parseInt(q);
        } catch (Exception e) {
        }
        if (qn < 1) qn = 1;
        if (qn > 500) qn = 500;

        JSONArray ja = new JSONArray();
        int i = 0;
        do {
            JSONObject jo = new JSONObject();
            ODocument d = con.queryDocument("SELECT FROM World WHERE id="+Math.random()*10000);
            if (d != null) {
                jo.put("id", (int)d.field("id"));
                jo.put("randomNumber", (int)d.field("randomNumber"));
                ja.put(jo);
                i++;
            }
        } while (i < qn);
        
        return ja.toString().getBytes();
    }

}
