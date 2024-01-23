package benchmark.repository;

import com.mongodb.MongoClientSettings;
import com.mongodb.ServerAddress;
import com.mongodb.client.MongoClient;
import com.mongodb.client.MongoClients;
import com.mongodb.client.MongoDatabase;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Collections;
import java.util.Properties;

public enum MongoDBFactory {

    INSTANCE;

    private final MongoDatabase db;

    MongoDBFactory() {

        String propertiesFileName = "/mongodb.properties";
        File propFile = new File(propertiesFileName);

        try (InputStream is = propFile.isFile() ?
                new FileInputStream(propFile) :
                this.getClass().getResourceAsStream(propertiesFileName)) {
            Properties prop = new Properties();
            prop.load(is);

            String host = prop.getProperty("host");
            int port = Integer.parseInt(prop.getProperty("port"));
            ServerAddress server = new ServerAddress(host, port);
            int maxPoolSize = DbFactory.INSTANCE.getMaxPoolSize(DbFactory.DbType.MONGODB);

            MongoClientSettings settings = MongoClientSettings.builder()
                    .applyToClusterSettings(builder -> builder.hosts(Collections.singletonList(server)))
                    .applyToConnectionPoolSettings(builder -> builder.maxSize(maxPoolSize))
                    .build();

            MongoClient client = MongoClients.create(settings);
            db = client.getDatabase(prop.getProperty("database"));
        } catch (IOException e) {
            throw new RuntimeException("Failed to read property file " + propertiesFileName);
        }
    }

    public MongoDatabase getDatabase() {
        return db;
    }
}
