<?php
if (!defined('CF_SYSTEM')) {
    exit('External script access not allowed');
}
/**
 * Cygnite PHP Framework
 *
 * Session Configuration
 *
 * @author Sanjoy Dey <dey.sanjoy0@gmail.com>
 */

return array(

    /**
     *----------------------------------------------------------
     * Session Driver
     *----------------------------------------------------------
     * Set session driver to use native session or database
     * based session. Just provide the driver as "native" or
     * "database". Cygnite will take care of next.
     */
    'driver' => 'native', // native, database

    /**
     *----------------------------------------------------------
     * Session Name
     *----------------------------------------------------------
     * Set your session name, else cygnite will use default name
     */
    'session_name'  => 'cf_secure_session',

    /**
     *----------------------------------------------------------
     * Session Storage Path
     *----------------------------------------------------------
     * Leave it as default as it will store into the default session
     * directory.
     */
    'path' => 'default',

    /**
     *----------------------------------------------------------
     * Database For Session Storage
     *----------------------------------------------------------
     * Set the database name if you are using database based session
     * in order to store session.
     *
     * Change database name below to use session database driver
     */
    'database_name' => 'cygnite',

    /**
     *----------------------------------------------------------
     * Setting Table Name For Session Storage
     *----------------------------------------------------------
     * Provide table name if you are using database based session.
     * By default "cf_sessions" is the table to store all session
     * into table.
     */
    'table' => 'cf_sessions',

    /**
     *----------------------------------------------------------
     * Use Session Cookie
     *----------------------------------------------------------
     * Configure to use session cookie or not
     */
    'use_session_cookie' => false,

    /**
     *----------------------------------------------------------
     * Session Cookie Name
     *----------------------------------------------------------
     * If you are using session cookie then provide the name here else
     * leave it blank.
     */
    'session_cookie_name' => '',

    /**
     *----------------------------------------------------------
     * HTTP Only
     *----------------------------------------------------------
     * Configure to set httponly for cookie params
     */
    'httponly' => true,

    /**
     * --------------------------------------------------------
     *  Set Session Time out
     * --------------------------------------------------------
     * Currently not implemented, you can change the session
     * timeout in php.ini configuration file
     */
    'timeout' => 1440,

    /**
     *----------------------------------------------------------
     * Secure Session
     *----------------------------------------------------------
     * You may leave it as false or true to use secure session
     * By default cygnite provides most security to the session.
     */
    'secure' => false,
);