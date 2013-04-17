package hello.web;

import hello.domain.World;

import org.hibernate.Session;
import org.hibernate.SessionFactory;
import org.hibernate.cfg.Configuration;
import org.hibernate.dialect.MySQLDialect;
import org.hibernate.service.ServiceRegistryBuilder;

public class HibernateUtil {

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
        Configuration configuration = configuration();
        configuration.setProperty("hibernate.dialect ", MySQLDialect.class.getName());
        configuration.setProperty("hibernate.cache.use_query_cache", "false");
        configuration.setProperty("show_sql", "false");
        configuration.addAnnotatedClass(World.class);
        ServiceRegistryBuilder serviceRegistryBuilder = new ServiceRegistryBuilder().applySettings(configuration.getProperties());
        return configuration.buildSessionFactory(serviceRegistryBuilder.buildServiceRegistry());
    }
    
    private static Configuration configuration() {
        boolean jndi = Boolean.parseBoolean(System.getProperty("jndi", "true"));
        Configuration configuration = new Configuration();
        if (jndi) {
            configuration.configure("/hibernate-jndi.cfg.xml");
        } else {
            configuration.configure("/hibernate-local.cfg.xml");
        }
        return configuration;
    }
    
}
