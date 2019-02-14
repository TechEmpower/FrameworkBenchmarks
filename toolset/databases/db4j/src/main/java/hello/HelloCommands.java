package hello;

import com.nqzero.orator.Orator;
import com.nqzero.orator.OratorUtils;
import com.nqzero.orator.OratorUtils.Taskable;
import hello.HelloData.Fortune;
import hello.HelloData.World;
import static hello.HelloData.randomWorldNumber;
import java.util.ArrayList;
import static java.util.Comparator.comparing;
import java.util.HashMap;
import kilim.Pausable;
import org.db4j.Btrees;
import org.db4j.Db4j;
import static hello.HelloData.schema;
import java.net.InetAddress;

/** commands to be sent to the orator middleware for accessing the database remotely */
public class HelloCommands {
    public static Db4j db4j() { return schema.db4j(); }
    public static Btrees.IK<World> worlds() { return schema.worlds; }
    public static Btrees.IK<Fortune> fortunes() { return schema.fortunes; }
  
    public static Orator net = new Orator();
    static InetAddress addy;
    static {
        net.init(0);
        try { addy = InetAddress.getByName("tfb-database"); }
        catch (Exception ex) {}
    }
    static int port = 8081;
    static OratorUtils.Remote roa = net.remotify(new OratorUtils.Remote().set(addy,0).inet,port);
    public static OratorUtils.Nest root = net.kellyify(0,true,roa);

    public static class GetWorld implements Taskable<World> {
        public World task() throws Pausable {
            return db4j().submit(txn
                    -> worlds().find(txn,randomWorldNumber())
            ).await().val;
        }
    }
    public static class GetWorlds implements Taskable<World []> {
        int num;

        public GetWorlds set(int num) { this.num = num; return this; }
        
        public World[] task() throws Pausable {
            var result = new World[num];
            var conn = db4j().connect();
            for (int ii = 0; ii<num; ii++) {
                int jj = ii;
                conn.submit(txn
                        -> result[jj] = worlds().find(txn,randomWorldNumber())
                );
            }
            conn.await();
            return result;
        }
    }
    public static class GetFortunes implements Taskable<ArrayList<Fortune>> {
        public ArrayList<Fortune> task() throws Pausable {
            var list = db4j().submit(txn
                    -> fortunes().getall(txn).vals()
            ).await().val;
            list.add(new Fortune(0,"Additional fortune added at request time."));
            list.sort(comparing(fortune -> fortune.message));
            return list;
        }
    }
    public static class GetAll implements Taskable<HashMap<Integer,Integer>> {
        public HashMap<Integer,Integer> task() throws Pausable {
            return db4j().submit(txn -> {
                HashMap<Integer,Integer> map = new HashMap();
                worlds().getall(txn).visit(cc -> map.put(cc.key,cc.val.randomNumber));
                return map;
            }).await().val;
        }
    }
    public static class UpdateWorlds extends GetWorlds {
        public World[] task() throws Pausable {
            var result = super.task();
            var conn = db4j().connect();
            for (var world : result) {
                world.randomNumber = randomWorldNumber();
                conn.submitCall(txn -> worlds().update(txn,world.id,world));
            }
            conn.await();
            return result;
        }
    }
    
    
}
