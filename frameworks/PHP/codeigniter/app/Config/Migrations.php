<?php

namespace Config;

use CodeIgniter\Config\BaseConfig;

class Migrations extends BaseConfig
{
    /**
     * --------------------------------------------------------------------------
     * Enable/Disable Migrations
     * --------------------------------------------------------------------------
     *
     * Migrations are enabled by default.
     *
     * You should enable migrations whenever you intend to do a schema migration
     * and disable it back when you're done.
     */
    public bool $enabled = true;

    /**
     * --------------------------------------------------------------------------
     * Migrations Table
     * --------------------------------------------------------------------------
     *
     * This is the name of the table that will store the current migrations state.
     * When migrations runs it will store in a database table which migration
     * level the system is at. It then compares the migration level in this
     * table to the $config['migration_version'] if they are not the same it
     * will migrate up. This must be set.
     */
    public string $table = 'migrations';

    /**
     * --------------------------------------------------------------------------
     * Timestamp Format
     * --------------------------------------------------------------------------
     *
     * This is the format that will be used when creating new migrations
     * using the CLI command:
     *   > php spark make:migration
     *
     * NOTE: if you set an unsupported format, migration runner will not find
     *       your migration files.
     *
     * Supported formats:
     * - YmdHis_
     * - Y-m-d-His_
     * - Y_m_d_His_
     */
    public string $timestampFormat = 'Y-m-d-His_';
}
