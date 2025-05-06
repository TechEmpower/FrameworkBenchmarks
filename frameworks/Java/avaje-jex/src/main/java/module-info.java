module avaje.techempower {
  // jdk.httpserver wrapper
  requires jdk.httpserver;
  requires io.avaje.jex;
  /// Postgres driver and pool
  requires java.sql;
  requires org.postgresql.jdbc;
  requires com.zaxxer.hikari;
  // template engine
  requires io.jstach.jstachio;
  /// Configuration
  requires io.avaje.config;
  // controller generation
  requires io.avaje.http.api;
  /// Dependency Injection
  requires io.avaje.inject;
  /// Json
  requires io.avaje.jsonb;
}
