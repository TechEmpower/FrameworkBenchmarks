<?php
namespace Cygnite\Database;

/**
 *  Cygnite Framework
 *  Database Migration Configuration Settings
 *
 *  An open source application development framework for PHP 5.3x or newer
 *
 *   License
 *
 *   This source file is subject to the MIT license that is bundled
 *   with this package in the file LICENSE.txt.
 *   http://www.cygniteframework.com/license.txt
 *   If you did not receive a copy of the license and are unable to
 *   obtain it through the world-wide-web, please send an email
 *   to sanjoy@hotmail.com so I can send you a copy immediately.
 *
 *@package               : Apps
 *@subpackages           : Database Migration Configurations
 *@filename              : migration.php
 *@description           : You can set your session configurations here.
 *@author                : Sanjoy Dey
 *@copyright             : Copyright (c) 2013 - 2014,
 *@link	                 : http://www.cygniteframework.com
 *@since	             : Version 1.2
 *@filesource
 *@warning               : If you don't protect this directory from direct web access,
 * anybody will be able to see your database configuration and settings.
 *
 *
 */

if (!defined('CF_SYSTEM')) {
    exit('External script access not allowed');
}

/**
 * Initialize your database configurations settings here.
 * You can connect with multiple database on the fly.
 * Don't worry about performance Cygnite will not
 * connect with database until first time you need your
 * connection to interact with database.
 * Specify your database name and table name in model to
 * do crude operations.
 *
 * <code>
 * 'connection'  => 'your-database',
 * 'migration_path' => 'migration-directory',
 * 'migration_table' => 'migration-table-name',
 *
 * </code>
 *
 * Example :
 * cd console/bin
 * php cygnite migrate:init create_products_table 
 * php cygnite migrate
 */


return array(
    'connection' => 'cygnite', //If you don't specify your connection name Cygnite will take default database 
    //connection to generate your migrations

    'migration_path' => 'apps.migrations',

    'migration_table' => 'migrations',

);
