/*
Copyright 2009-2010 Igor Polevoy 

Licensed under the Apache License, Version 2.0 (the "License"); 
you may not use this file except in compliance with the License. 
You may obtain a copy of the License at 

http://www.apache.org/licenses/LICENSE-2.0 

Unless required by applicable law or agreed to in writing, software 
distributed under the License is distributed on an "AS IS" BASIS, 
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. 
See the License for the specific language governing permissions and 
limitations under the License. 
*/
package app.config;

import org.javalite.activeweb.AbstractDBConfig;
import org.javalite.activeweb.AppContext;

/**
 * @author Igor Polevoy
 */
public class DbConfig extends AbstractDBConfig {
    public void init(AppContext context) {

        String jdbcParams = "jdbcCompliantTruncation=false&elideSetAutoCommits=true" +
                "&useLocalSessionState=true" +
                "&cachePrepStmts=true" +
                "&cacheCallableStmts=true" +
                "&alwaysSendSetIsolation=false" +
                "&prepStmtCacheSize=4096" +
                "&cacheServerConfiguration=true" +
                "&prepStmtCacheSqlLimit=2048" +
                "&zeroDateTimeBehavior=convertToNull" +
                "&traceProtocol=false" +
                "&useUnbufferedInput=false" +
                "&useReadAheadInput=false" +
                "&maintainTimeStats=false" +
                "&useServerPrepStmts" +
                "&cacheRSMetadata=true";

        environment("development").jndi("java:comp/env/jdbc/hello_world");

        //need to set ACTIVE_ENV=local to run on dev box.
        environment("local").jdbc("com.mysql.jdbc.Driver", "jdbc:mysql://localhost/hello_world?" + jdbcParams, "benchmarkdbuser", "benchmarkdbpass");

        environment("development").testing().jdbc("com.mysql.jdbc.Driver", "jdbc:mysql://localhost/hello_world?" + jdbcParams, "benchmarkdbuser", "benchmarkdbpass");
    }
}
