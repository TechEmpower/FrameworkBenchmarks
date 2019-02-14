package hello;

import com.nqzero.orator.OratorUtils.Taskable;
import com.samskivert.mustache.Mustache;
import com.samskivert.mustache.Template;
import hello.HelloCommands.GetAll;
import hello.HelloCommands.GetFortunes;
import hello.HelloCommands.GetWorld;
import hello.HelloCommands.GetWorlds;
import hello.HelloCommands.UpdateWorlds;
import static hello.HelloCommands.net;
import static hello.HelloCommands.root;
import java.text.SimpleDateFormat;

import java.util.Map;
import kilim.Mailbox;
import kilim.Pausable;
import kilim.http.HttpRequest;
import kilim.http.HttpResponse;
import static kilim.http.HttpResponse.gmtdf;
import kilim.http.KilimMvc;
import kilim.http.KilimMvc.Router;
import kilim.http.KilimMvc.Session;
import kilim.tools.Kilim;
import org.db4j.Db4j;
import org.db4j.Db4jMvc;
import org.db4j.Db4jMvc.Db4jRouter;
import static org.db4j.Db4jMvc.badRequest;
import static org.db4j.perf.DemoHunker.resolve;

public final class HelloController extends Db4jRouter<HelloController> {
    HelloController(KilimMvc.Clerk mk) { super(mk); }

    public static String fortune = "<!DOCTYPE html>\n"
            +"<html>\n"
            +"<head>\n"
            +"  <title>Fortunes</title>\n"
            +"</head>\n"
            +"<body>\n"
            +"<table>\n"
            +"  <tr>\n"
            +"    <th>id</th>\n"
            +"    <th>message</th>\n"
            +"  </tr>\n"
            +"  {{#.}}\n"
            +"  <tr>\n"
            +"    <td>{{id}}</td>\n"
            +"    <td>{{message}}</td>\n"
            +"  </tr>\n"
            +"  {{/.}}\n"
            +"</table>\n"
            +"</body>\n"
            +"</html>";
    
    public static Template template = Mustache.compiler().compile(fortune);

    
    
    
    <TT> TT deferTask(Taskable<TT> body) throws Pausable {
        Mailbox<TT> box = net.sendTask(body,root);
        return box.get();
    }
    
    { add("/plaintext",this::plaintext); }
    Text plaintext() throws Pausable {
        return new Text("Hello, World!");
    }

    { add("/json",this::json); }
    Map<String,String> json() throws Pausable {
        return Map.of("message","Hello, World!");
    }

    { add("/db",this::db); }
    Object db() throws Pausable {
        return deferTask(new GetWorld());
    }

    {
        add("/all",() -> deferTask(new GetAll()));
    }

    
    { add("/queries?queries",this::queries); }
    Object queries(String queries) throws Pausable {
        int num = parseQueryCount(queries);
        return deferTask(new GetWorlds().set(num));
    }

    { add("/updates?queries",this::updates); }
    Object updates(String queries) throws Pausable {
        int num = parseQueryCount(queries);
        return deferTask(new UpdateWorlds().set(num));
    }

    { add("/fortunes",this::fortunes); }
    Object fortunes() throws Pausable {
        var box = net.sendTask(new GetFortunes(),root);
        var list = box.get();
        String html = template.execute(list);
        return new Html(html);
    }

    { make0(new KilimMvc.Route(),self -> self::fallback); }
    Object fallback() throws Pausable {
        throw badRequest("not found");
    }

    static class Html { String body; Html(String $body) { body=$body; }}
    static class Text { String body; Text(String $body) { body=$body; }}
    
    public static class Mvc extends Db4jMvc {
        Mvc() {
            super(x -> new HelloController(x),pp -> {});
        }

        public void handle(Session session,HttpRequest req,HttpResponse resp) throws Pausable, Exception {
            Object reply;

            try { reply = route(session,req,resp); }
            catch (HttpStatus ex) { reply = handleEx(session,req,resp,ex); }

            if (reply instanceof Html) {
                Html html = (Html) reply;
                resp.setContentType("text/html; charset=utf-8");
                resp.getOutputStream().write(html.body.getBytes());
                session.sendResponse(resp);
            }
            else if (reply instanceof Text) {
                Text html = (Text) reply;
                resp.setContentType("text/plain");
                resp.getOutputStream().write(html.body.getBytes());
                session.sendResponse(resp);
            }
            else if (reply != null) {
                write(resp,reply);
                session.sendResponse(resp);
            }
        }
    }

    
    private static int parseQueryCount(String textValue) {
        if (textValue==null)
            return 1;
        int parsedValue;
        try {
            parsedValue = Integer.parseInt(textValue);
        } catch (NumberFormatException e) {
            return 1;
        }
        return Math.min(500,Math.max(1,parsedValue));
    }

    public static void main(String[] args) throws Exception {
        new Mvc().start(8080);
    }
}
