package app;

import com.google.inject.AbstractModule;

import app.controllers.DbController;
import app.db.DbManager;
import app.models.Message;
import net.javapla.jawn.core.Jawn;
import net.javapla.jawn.core.Results;
import net.javapla.jawn.core.server.ServerConfig.PERFORMANCE_MODE;
import net.javapla.jawn.core.util.Modes;

public class BenchmarkMain extends Jawn {
    private static final String message = "Hello, World!";
    private static final byte[] bytemessage = message.getBytes();
    

    String host = "tfb-database:5432";
    String dbUrl = "jdbc:postgresql://"+host+"/hello_world";
    
    // implicit constructor
    {
        env(Modes.PROD);
        server()
            .port(8080)
            .webappPath("webapp")
            .serverPerformance(PERFORMANCE_MODE.HIGHEST);
        
        
        get("/queries",DbController.class, DbController::getQueries);
        get("/updates",DbController.class, DbController::getUpdates);
        
        get("/json", (context) -> Results.json(new Message(message)).addHeader("Server", "jawn"));
        get("/plaintext", (context) -> Results.text(bytemessage).addHeader("Server", "jawn"));
        
        use(new AbstractModule() {
            @Override
            protected void configure() {
                bind(DbManager.class);
            }
        });
        
        database(Modes.PROD)
            .jdbc()
            .driver("org.postgresql.Driver")
            .url(dbUrl)
            .user("benchmarkdbuser")
            .password("benchmarkdbpass")
            .maxPoolSize(32)
            .letFrameworkHandleConnectionPool(true);
        
        database(Modes.TEST)
            .jdbc()
            .driver("org.postgresql.Driver")
            .url("jdbc:postgresql://172.16.0.16/hello_world")
            .user("benchmarkdbuser")
            .password("benchmarkdbpass")
            .letFrameworkHandleConnectionPool(true);
    
        database(Modes.DEV)
            .jdbc()
            .driver("org.postgresql.Driver")
            .url("jdbc:postgresql://172.16.0.16:5432/hello_world")
            .user("benchmarkdbuser")
            .password("benchmarkdbpass")
            .letFrameworkHandleConnectionPool(true);
    }

    public static void main(String[] args) throws Exception {
        run(BenchmarkMain::new, args);
    }
}
