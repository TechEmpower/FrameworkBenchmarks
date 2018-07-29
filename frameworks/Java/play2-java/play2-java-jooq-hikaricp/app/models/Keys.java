/*
 * This file is generated by jOOQ.
*/
package models;


import javax.annotation.Generated;

import models.tables.Fortune;
import models.tables.World;
import models.tables.records.FortuneRecord;
import models.tables.records.WorldRecord;

import org.jooq.Identity;
import org.jooq.UniqueKey;
import org.jooq.impl.AbstractKeys;
import org.jooq.types.UInteger;


/**
 * A class modelling foreign key relationships and constraints of tables of 
 * the <code>hello_world</code> schema.
 */
@Generated(
    value = {
        "http://www.jooq.org",
        "jOOQ version:3.10.3"
    },
    comments = "This class is generated by jOOQ"
)
@SuppressWarnings({ "all", "unchecked", "rawtypes" })
public class Keys {

    // -------------------------------------------------------------------------
    // IDENTITY definitions
    // -------------------------------------------------------------------------

    public static final Identity<FortuneRecord, UInteger> IDENTITY_FORTUNE = Identities0.IDENTITY_FORTUNE;
    public static final Identity<WorldRecord, UInteger> IDENTITY_WORLD = Identities0.IDENTITY_WORLD;

    // -------------------------------------------------------------------------
    // UNIQUE and PRIMARY KEY definitions
    // -------------------------------------------------------------------------

    public static final UniqueKey<FortuneRecord> KEY_FORTUNE_PRIMARY = UniqueKeys0.KEY_FORTUNE_PRIMARY;
    public static final UniqueKey<WorldRecord> KEY_WORLD_PRIMARY = UniqueKeys0.KEY_WORLD_PRIMARY;

    // -------------------------------------------------------------------------
    // FOREIGN KEY definitions
    // -------------------------------------------------------------------------


    // -------------------------------------------------------------------------
    // [#1459] distribute members to avoid static initialisers > 64kb
    // -------------------------------------------------------------------------

    private static class Identities0 extends AbstractKeys {
        public static Identity<FortuneRecord, UInteger> IDENTITY_FORTUNE = createIdentity(Fortune.FORTUNE, Fortune.FORTUNE.ID);
        public static Identity<WorldRecord, UInteger> IDENTITY_WORLD = createIdentity(World.WORLD, World.WORLD.ID);
    }

    private static class UniqueKeys0 extends AbstractKeys {
        public static final UniqueKey<FortuneRecord> KEY_FORTUNE_PRIMARY = createUniqueKey(Fortune.FORTUNE, "KEY_fortune_PRIMARY", Fortune.FORTUNE.ID);
        public static final UniqueKey<WorldRecord> KEY_WORLD_PRIMARY = createUniqueKey(World.WORLD, "KEY_world_PRIMARY", World.WORLD.ID);
    }
}
