package app.db;

import com.google.inject.AbstractModule;

public class DbModule extends AbstractModule {

    @Override
    protected void configure() {
        bind(DbManager.class);
    }

}
