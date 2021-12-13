package app;

import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;
import java.util.Collections;
import java.util.List;

import app.db.DbManager;
import app.helpers.Helper;
import app.models.Fortune;
import app.models.Message;
import net.javapla.jawn.core.Jawn;
import net.javapla.jawn.core.Results;
import net.javapla.jawn.core.server.ServerConfig.Performance;
import net.javapla.jawn.core.util.Modes;

public class BenchmarkMain extends Jawn {
    private static final String message = "Hello, World!";
    private static final byte[] bytemessage = message.getBytes(StandardCharsets.UTF_8);
    private static final ByteBuffer buffermessage = ByteBuffer.wrap(bytemessage);
    
    private static DbManager db;
    
    // implicit constructor
    {
        mode(Modes.DEV);
        server()
            .port(8080)
            .performance(Performance.HIGHEST);
        
        
        onStartup(() -> db = require(DbManager.class)); // save a reference to the database when everything has started
        
        
        
        // Messages
        
        get("/json", () -> Results.json(new Message(message)));
        get("/plaintext", () -> Results.text(buffermessage.duplicate()));
        

        // Fortunes
        
        get("/fortunes", () -> {
            List<Fortune> fortunes = db.fetchAllFortunes();
            fortunes.add(new Fortune(0, "Additional fortune added at request time."));
            Collections.sort(fortunes, (f1, f2) -> f1.message.compareTo(f2.message));
            
            return Results.view().put("fortunes", fortunes); // implicitly reads /webapp/views/index.html.st
        });
        
        
        // Worlds
        
        get("/db", () -> Results.json(db.getWorld(Helper.getRandomNumber())) );
        
        get("/queries", ctx -> {
            int q = ctx.req().queryParam("queries").intValue(1);
            return Results.json( db.getWorlds( q > 500 ? 500 : q < 1 ? 1 : q ));
        });
        
        get("/updates", ctx -> {
            int q = ctx.req().queryParam("queries").intValue(1);
            return Results.json(db.getAndUpdateWorlds( q > 500 ? 500 : q < 1 ? 1 : q ));
        });
    }

    public static void main(String[] args) throws Exception {
        run(BenchmarkMain.class, args);
    }
}
