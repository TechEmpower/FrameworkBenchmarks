
import com.orientechnologies.orient.core.record.impl.ODocument;
import java.util.HashMap;
import permeagility.util.DatabaseConnection;
import permeagility.web.Download;

/** Test type 2: Single Database Query
 */
public final class db extends Download {

    @Override public String getContentType() { return "application/json"; }
    
    @Override public String getContentDisposition() { return null; }
    
    @Override public byte[] getBytes(DatabaseConnection con, HashMap<String, String> parms) {
        ODocument d = con.queryDocument("SELECT FROM World WHERE id="+Math.random()*10000);
        if (d != null) {
            return d.toJSON("").getBytes();
        } else {
            return null;
        }
    }

}
