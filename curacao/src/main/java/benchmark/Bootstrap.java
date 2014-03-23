package benchmark;

import com.kolich.curacao.CuracaoContextListener;
import com.kolich.curacao.CuracaoDispatcherServlet;
import org.eclipse.jetty.server.Server;
import org.eclipse.jetty.servlet.ServletHolder;
import org.eclipse.jetty.webapp.WebAppContext;

import java.io.File;
import java.nio.file.Path;
import java.nio.file.Paths;

public final class Bootstrap {

    private static final int DEFAULT_SERVER_LISTEN_PORT = 8080;

    public static void main(final String[] args) throws Exception {

        final File workingDir = getWorkingDir();

        int port;
        try {
            port = Integer.parseInt(args[0]);
        } catch (Exception e) {
            port = DEFAULT_SERVER_LISTEN_PORT;
        }

        final Server server = new Server(port);

        final ServletHolder holder = new ServletHolder(CuracaoDispatcherServlet.class);
        holder.setAsyncSupported(true); // Async supported = true
        holder.setInitOrder(1); // Load on startup = true

        final WebAppContext context = new WebAppContext();
        context.addEventListener(new CuracaoContextListener()); // Required
        context.setContextPath("/");
        context.setResourceBase(workingDir.getAbsolutePath());
        context.addServlet(holder, "/*");

        server.setHandler(context);

        server.start();
        server.join();

    }

    private static final File getWorkingDir() {
        final Path currentRelativePath = Paths.get("");
        return currentRelativePath.toAbsolutePath().toFile();
    }

}
