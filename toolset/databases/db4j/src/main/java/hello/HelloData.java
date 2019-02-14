package hello;

import com.nqzero.orator.Orator;
import java.util.concurrent.ThreadLocalRandom;
import org.db4j.Btrees;
import org.db4j.Database;
import org.db4j.Db4j;

/** the schema and support classes for the techempower benchmark examples */
public class HelloData extends Database {
    public static HelloData schema;

    Btrees.IK<World> worlds;
    Btrees.IK<Fortune> fortunes;

    public static final class Fortune {
        public int id;
        public String message;

        public Fortune() {}
        public Fortune(int id,String message) {
            this.id = id;
            this.message = message;
        }
  }

    public static final class World {
        public int id;
        public int randomNumber;

        public World() {}
        public World(int id,int randomNumber) {
            this.id = id;
            this.randomNumber = randomNumber;
        }
    }

    public Db4j db4j() { return db4j; }

    static int randomWorldNumber() {
        return 1+ThreadLocalRandom.current().nextInt(10000);
    }
  

    void make() {
        try { Thread.sleep(1000); } catch (Exception ex) {}
        makeWorld();
        makeFortunes();
    }
    void makeWorld() {
        Db4j.Connection conn = db4j.connect();
        for (int ii = 0; ii<10000; ii++) {
            int jj = ii;
            conn.submitCall(txn -> worlds.insert(txn,1+jj,new World(1+jj,randomWorldNumber())));
        }
        conn.awaitb();
    }
  
    void makeFortune(Db4j.Connection conn,int id,String val) {
        conn.submitCall(txn -> fortunes.insert(txn,id,new Fortune(id,val)));
    }
  
    void makeFortunes() {
        Db4j.Connection conn = db4j.connect();
        makeFortune(conn,1,"fortune: No such file or directory");
        makeFortune(conn,2,"A computer scientist is someone who fixes things that aren't broken.");
        makeFortune(conn,3,"After enough decimal places, nobody gives a damn.");
        makeFortune(conn,4,"A bad random number generator: 1, 1, 1, 1, 1, 4.33e+67, 1, 1, 1");
        makeFortune(conn,5,"A computer program does what you tell it to do, not what you want it to do.");
        makeFortune(conn,6,"Emacs is a nice operating system, but I prefer UNIX. — Tom Christaensen");
        makeFortune(conn,7,"Any program that runs right is obsolete.");
        makeFortune(conn,8,"A list is only as strong as its weakest link. — Donald Knuth");
        makeFortune(conn,9,"Feature: A bug with seniority.");
        makeFortune(conn,10,"Computers make very fast, very accurate mistakes.");
        makeFortune(conn,11,"<script>alert(\"This should not be displayed in a browser alert box.\");</script>");
        makeFortune(conn,12,"フレームワークのベンチマーク");
        conn.awaitb();
    }

    public HelloData() {
        this(false);
    }
    public HelloData(boolean build) {
        start("./hello.db4j",build);
        if (build) make();
    }

    public static void main(String[] args) throws Exception {
        boolean build = args.length>0;
        HelloData hello = new HelloData(build);
        schema = hello;
        new Orator().init(8081);
    }
}
