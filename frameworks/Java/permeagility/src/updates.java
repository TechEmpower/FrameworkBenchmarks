import com.orientechnologies.orient.core.metadata.schema.OType;
import com.orientechnologies.orient.core.record.impl.ODocument;
import java.util.HashMap;
import permeagility.plus.json.JSONArray;
import permeagility.plus.json.JSONObject;
import permeagility.util.DatabaseConnection;
import permeagility.web.Download;

/** Test type 5: Database updates
 */
public final class updates extends Download {

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
        for (int i=0; i<qn; i++) {
            JSONObject jo = new JSONObject();
            ODocument d = con.queryDocument("SELECT FROM World WHERE id="+Math.random()*10000);
            if (d != null) {
                int id = (int)d.field("id", OType.INTEGER);
                int newRand = (int)(Math.random()*10000);
//                con.update("UPDATE "+d.getIdentity().toString()+" SET randomNumber=" + newRand + " LOCK RECORD");
                d.field("randomNumber",newRand).save();
                jo.put("id", id);
                jo.put("randomNumber", newRand);
                ja.put(jo);
            }
        }
        return ja.toString().getBytes();
    }

}
