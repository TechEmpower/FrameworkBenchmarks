package hello.web;

import hello.domain.World;

import org.hibernate.Session;
import org.hibernate.SessionFactory;
import org.hibernate.boot.registry.StandardServiceRegistryBuilder;
import org.hibernate.cfg.AvailableSettings;
import org.hibernate.cfg.Configuration;
import org.hibernate.dialect.MySQLDialect;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class HibernateUtil {

    private static final Logger LOGGER = LoggerFactory.getLogger(HibernateUtil.class);
    
    private static final SessionFactory SESSION_FACTORY = createSessionFactory();
    private static final ThreadLocal<Session> SESSIONS = new ThreadLocal<>();
    
    public static Session getSession() {
        Session session = SESSIONS.get();
        if (session == null) {
            session = SESSION_FACTORY.openSession();
            SESSIONS.set(session);
        }
        return session;
    }
    
    public static void closeSession() {
        Session session = SESSIONS.get();
        if (session != null) {
            session.close();
            SESSIONS.remove();
        }
    }
    
    private static SessionFactory createSessionFactory() {
        try {
            Configuration configuration = configuration();
            String url = configuration.getProperty(AvailableSettings.URL);
            configuration.setProperty(AvailableSettings.URL, url.replace("{db-host}", "localhost"));
            configuration.setProperty(AvailableSettings.DIALECT, MySQLDialect.class.getName());
            configuration.setProperty(AvailableSettings.USE_QUERY_CACHE, "false");
            configuration.setProperty(AvailableSettings.SHOW_SQL, "false");
            configuration.setProperty(AvailableSettings.CURRENT_SESSION_CONTEXT_CLASS, "thread");
            configuration.addAnnotatedClass(World.class);
            StandardServiceRegistryBuilder serviceRegistryBuilder = new StandardServiceRegistryBuilder().applySettings(configuration.getProperties());
            return configuration.buildSessionFactory(serviceRegistryBuilder.build());
        } catch (RuntimeException ex) {
            LOGGER.error("Failed to create session factory");
            throw ex;
        }
    }
    
    private static Configuration configuration() {
        boolean jndi = Boolean.parseBoolean(System.getProperty("jndi", "true"));
        Configuration configuration = new Configuration();
        // We're always going to use the -local config now since there were previous 
        // problems with the jndi config.
        /*
        if (jndi) {
            configuration.configure("/hibernate-jndi.cfg.xml");
        } else {
            configuration.configure("/hibernate-local.cfg.xml");
        }
        */
        configuration.configure("/hibernate-local.cfg.xml");
        return configuration;
    }
    
}
