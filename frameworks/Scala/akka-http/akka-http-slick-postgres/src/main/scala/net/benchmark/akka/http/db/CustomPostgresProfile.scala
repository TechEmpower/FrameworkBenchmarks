package net.benchmark.akka.http.db

import com.github.tminglei.slickpg._
import slick.basic.Capability
import slick.jdbc.JdbcCapabilities

trait CustomPostgresProfile extends ExPostgresProfile with PgArraySupport with PgDateSupport with PgDate2Support {

  // Add back `capabilities.insertOrUpdate` to enable native `upsert` support; for postgres 9.5+
  override protected def computeCapabilities: Set[Capability] =
    super.computeCapabilities + JdbcCapabilities.insertOrUpdate

  // IMPORTANT: Do not add a type annotation here or you'll trigger an error. See https://github.com/tminglei/slick-pg/issues/303#issuecomment-252403085
  override val api = MyAPI

  object MyAPI extends API with ArrayImplicits with SimpleDateTimeImplicits with DateTimeImplicits
}

/**
  * The actual postgresql profile useable by the application.
  */
object CustomPostgresProfile extends CustomPostgresProfile
