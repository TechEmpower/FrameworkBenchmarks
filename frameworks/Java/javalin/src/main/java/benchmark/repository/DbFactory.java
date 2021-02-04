package benchmark.repository;

public enum DbFactory {

    INSTANCE;

    static final String PHYSICAL_TAG = "Citrine";
    static final String CLOUD_TAG = "Azure";

    private static final int POSTGRES_PHYSICAL_POOL_SIZE = 112;
    private static final int POSTGRES_CLOUD_POOL_SIZE = 16;
    private static final int POSTGRES_DEFAULT_POOL_SIZE = 10;

    private static final int MONGODB_PHYSICAL_POOL_SIZE = 200;
    private static final int MONGODB_CLOUD_POOL_SIZE = 100;
    private static final int MONGODB_DEFAULT_POOL_SIZE = 50;

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

        switch (dbType) {
            case POSTGRES:
                if (PHYSICAL_TAG.equals(env)) {
                    maxPoolSize = POSTGRES_PHYSICAL_POOL_SIZE;
                } else if (CLOUD_TAG.equals(env)) {
                    maxPoolSize = POSTGRES_CLOUD_POOL_SIZE;
                } else {
                    maxPoolSize = POSTGRES_DEFAULT_POOL_SIZE;
                }
                break;
            case MONGODB:
                if (PHYSICAL_TAG.equals(env)) {
                    maxPoolSize = MONGODB_PHYSICAL_POOL_SIZE;
                } else if (CLOUD_TAG.equals(env)) {
                    maxPoolSize = MONGODB_CLOUD_POOL_SIZE;
                } else {
                    maxPoolSize = MONGODB_DEFAULT_POOL_SIZE;
                }
                break;
            default:
                maxPoolSize = 100;
        }

        return maxPoolSize;
    }
}
