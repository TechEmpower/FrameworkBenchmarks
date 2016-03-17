
import java.util.HashMap;
import permeagility.util.DatabaseConnection;
import permeagility.web.Download;

/** Test type 6: Plaintext
 */
public final class plaintext extends Download {

    @Override public String getContentType() { return "text/plain"; }
    
    @Override public String getContentDisposition() { return null; }
    
    @Override public byte[] getBytes(DatabaseConnection con, HashMap<String, String> parms) {
	return "Hello, World!".getBytes();
    }

}
