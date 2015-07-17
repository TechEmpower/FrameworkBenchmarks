package hello;

import co.paralleluniverse.embedded.containers.AbstractEmbeddedServer;
import org.apache.catalina.Context;
import org.apache.catalina.LifecycleException;
import org.apache.catalina.Wrapper;
import org.apache.catalina.core.StandardContext;
import org.apache.catalina.startup.Tomcat;
import org.apache.tomcat.websocket.server.WsSci;

import javax.servlet.Servlet;
import javax.servlet.ServletContextListener;
import java.io.File;
import java.util.Set;

public class TomcatServer extends AbstractEmbeddedServer {
	private static final String defaultResDir = System.getProperty(TomcatServer.class.getName() + ".defaultResDir", "./build");

	private final Tomcat tomcat;
	private Context context;

	public TomcatServer() {
		this(defaultResDir);
	}

	public TomcatServer(String resDir) {
		this.tomcat = new Tomcat();
		this.context = tomcat.addContext("/", new File(resDir).getAbsolutePath());
	}

	public ServletDesc addServlet(String name, Class<? extends Servlet> servletClass, String mapping) {
		Wrapper w = Tomcat.addServlet(this.context, name, servletClass.getName());
		w.addMapping(mapping);
		return new TomcatServer.TomcatServletDesc(w);
	}

	public void start() throws Exception {
		this.tomcat.setPort(this.port);
		this.tomcat.getConnector().setAttribute("maxThreads", Integer.valueOf(this.nThreads));
		this.tomcat.getConnector().setAttribute("acceptCount", Integer.valueOf(this.maxConn));
		Runtime.getRuntime().addShutdownHook(new Thread() {
			@Override
			public void run() {
				try {
					tomcat.stop();
				} catch (LifecycleException e) {
					throw new RuntimeException(e);
				}
			}
		});
		this.tomcat.start();
		new Thread() {
			@Override
			public void run() {
				tomcat.getServer().await();
			}
		}.start();
	}

	public void stop() throws Exception {
		this.tomcat.stop();
		this.tomcat.getConnector().destroy();
		this.tomcat.destroy();
	}

	public void enableWebsockets() throws Exception {
		this.context.addServletContainerInitializer(new WsSci(), (Set)null);
	}

	public void addServletContextListener(Class<? extends ServletContextListener> scl) {
		StandardContext tomcatCtx = (StandardContext)this.context;
		tomcatCtx.addApplicationListener(scl.getName());
	}

	public void setResourceBase(String resourceBaseUrl) {
		throw new UnsupportedOperationException("Not supported yet.");
	}

	private static class TomcatServletDesc implements ServletDesc {
		private final Wrapper impl;

		public TomcatServletDesc(Wrapper w) {
			this.impl = w;
		}

		public ServletDesc setInitParameter(String name, String value) {
			this.impl.addInitParameter(name, value);
			return this;
		}

		public ServletDesc setLoadOnStartup(int load) {
			this.impl.setLoadOnStartup(load);
			return this;
		}
	}
}
