package com.litongjava.tio.http.server.config;

import com.litongjava.db.activerecord.ActiveRecordPlugin;
import com.litongjava.db.activerecord.OrderedFieldContainerFactory;
import com.litongjava.db.hikaricp.HikariCpPlugin;
import com.litongjava.tio.utils.environment.EnvUtils;

public class MysqlDbConfig {

  public void init() {
    // start active recored
    String jdbcUrl = EnvUtils.get("JDBC_URL");
    // String jdbcUrl = "jdbc:mysql://192.168.3.9/hello_world";

    String jdbcUser = EnvUtils.get("JDBC_USER");
    // String jdbcUser = "root";

    String jdbcPswd = EnvUtils.get("JDBC_PSWD");
    // String jdbcPswd = "robot_123456#";
    HikariCpPlugin hikariCpPlugin = new HikariCpPlugin(jdbcUrl, jdbcUser, jdbcPswd);

    ActiveRecordPlugin arp = new ActiveRecordPlugin(hikariCpPlugin);
    arp.setContainerFactory(new OrderedFieldContainerFactory());

    // arp.setShowSql(true);

    hikariCpPlugin.start();
    boolean start = arp.start();
    System.out.println("db started:" + start);
  }
}
