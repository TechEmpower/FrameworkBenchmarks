package benchmark.repository;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;

public enum DbFactory {

    INSTANCE;

    public enum DbType {POSTGRES, MONGODB}

    public DbService getDbService(DbType type) {

        DbService dbService;

        switch(type) {
            case POSTGRES:
                dbService=  new JDBCDbService();
                break;
            case MONGODB:
                dbService = new MongoDbService();
                break;
            default:
                dbService = null;
        }

        return dbService;
    }

    public int getMaxPoolSize(DbType dbType) {

        int maxPoolSize;
        String env = System.getenv("BENCHMARK_ENV");
        String propertiesFileName = "/environment.properties";
        File propFile = new File(propertiesFileName);

        try (InputStream is = propFile.isFile() ?
                new FileInputStream(propFile) :
                this.getClass().getResourceAsStream(propertiesFileName)) {
            Properties prop = new Properties();
            prop.load(is);

            switch (dbType) {
                case POSTGRES:
                    if (prop.getProperty("physicalTag").equals(env)) {
                        maxPoolSize = Integer.parseInt(prop.getProperty("postgresPhysicalPoolSize"));
                    } else if (prop.getProperty("cloudTag").equals(env)) {
                        maxPoolSize = Integer.parseInt(prop.getProperty("postgresCloudPoolSize"));
                    } else {
                        maxPoolSize = Integer.parseInt(prop.getProperty("postgresDefaultPoolSize"));
                    }
                    break;
                case MONGODB:
                    if (prop.getProperty("physicalTag").equals(env)) {
                        maxPoolSize = Integer.parseInt(prop.getProperty("mongodbPhysicalPoolSize"));
                    } else if (prop.getProperty("cloudTag").equals(env)) {
                        maxPoolSize = Integer.parseInt(prop.getProperty("mongodbCloudPoolSize"));
                    } else {
                        maxPoolSize = Integer.parseInt(prop.getProperty("mongodbDefaultPoolSize"));
                    }
                    break;
                default:
                    maxPoolSize = 100;
            }
        } catch (IOException | NumberFormatException e) {
            throw new RuntimeException("Failed to read property file " + propertiesFileName);
        }

        return maxPoolSize;
    }
}
