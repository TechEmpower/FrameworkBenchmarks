<?php
/**
 * Part of the Fuel framework.
 *
 * @package    Fuel
 * @version    1.5
 * @author     Fuel Development Team
 * @license    MIT License
 * @copyright  2010 - 2013 Fuel Development Team
 * @link       http://fuelphp.com
 */

/**
 * NOTICE:
 *
 * If you need to make modifications to the default configuration, copy
 * this file to your app/config folder, and make them in there.
 *
 * This will allow you to upgrade fuel without losing your custom config.
 */


return array(

	// The default File_Area config
	'base_config' => array(

		/**
		 * Path to basedir restriction, null for no restriction
		 */
		'basedir'  => null,

		/**
		 * array of allowed extensions, null for all
		 */
		'extensions'  => null,

		/**
		 * Base url for files, null for not available
		 */
		'url'  => null,

		/**
		 * Whether or not to use file locks when doing file operations
		 */
		'use_locks'  => null,

		/**
		 * array containing file driver per file extension
		 */
		'file_handlers'  => array(),
	),

	// Pre configure some areas
	'areas' => array(
		/* 'area_name' => array(
			'basedir'        => null,
			'extensions'     => null,
			'url'            => null,
			'use_locks'      => null,
			'file_handlers'  => array(),
		), */
	),

	// fileinfo() magic filename
	'magic_file' => null,

	// default file and directory permissions
	'chmod' => array(

		/**
		 * Permissions for newly created files
		 */
		'files'  => 0666,

		/**
		 * Permissions for newly created directories
		 */
		'folders'  => 0777,
	),

);


