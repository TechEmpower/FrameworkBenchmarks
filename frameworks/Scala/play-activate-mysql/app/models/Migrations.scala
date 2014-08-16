package models

import persistenceContext._

class CreateSchema extends Migration {

    def timestamp = System.currentTimeMillis + 100

    def up = {
        removeAllEntitiesTables.cascade.ifExists
        createTableForAllEntities.ifNotExists
    }

}

class MigrateFortunes extends Migration {

    def timestamp = System.currentTimeMillis + 200

    def up = {
        customScript {
            val con = storage.directAccess
            try {
                val rs = con.createStatement.executeQuery("SELECT id, message FROM Fortune")
                while (rs.next)
                    new ActivateFortune(rs.getLong(1), rs.getString(2))
            } finally
                con.close
        }
    }

}

class MigrateWorlds extends Migration {

    def timestamp = System.currentTimeMillis + 300

    def up = {
        customScript {
            val con = storage.directAccess
            try {
                val rs = con.createStatement.executeQuery("SELECT id, randomNumber FROM World")
                while (rs.next)
                    new ActivateWorld(rs.getLong(1), rs.getInt(2))
            } finally
                con.close
        }
    }

}

class CreateVersionIndexes extends Migration {
    
    def timestamp = System.currentTimeMillis + 400
    
    def up = {
        customScript {
            val con = storage.directAccess
            try {
                con.createStatement.executeUpdate("CREATE UNIQUE INDEX IX_WORLD_VERSION ON ActivateWorld(id, version)")
                con.createStatement.executeUpdate("CREATE UNIQUE INDEX IX_FORTUNE_VERSION ON ActivateFortune(id, version)")
            } finally
                con.close
        }
    }
}
