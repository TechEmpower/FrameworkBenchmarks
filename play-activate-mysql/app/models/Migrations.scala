package models

import persistenceContext._

class CreateSchema extends Migration {

    def timestamp = System.currentTimeMillis + 1

    def up = {
        removeAllEntitiesTables.cascade.ifExists
        createTableForAllEntities.ifNotExists
    }

}

class MigrateFortunes extends Migration {

    def timestamp = System.currentTimeMillis + 2

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

    def timestamp = System.currentTimeMillis + 3

    def up = {
        customScript {
            val con = storage.directAccess
            try {
                val rs = con.createStatement.executeQuery("SELECT id, randomNumber FROM World")
                while (rs.next)
                    new ActivateWorld(rs.getLong(1), rs.getInt(1))
            } finally
                con.close
        }
    }

}