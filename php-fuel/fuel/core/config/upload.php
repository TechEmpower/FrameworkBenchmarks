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
	/**
	 * global configuration
	*/

	// if true, the $_FILES array will be processed when the class is loaded
	'auto_process'		=> true,

	/**
	 * file validation settings
	*/

	// maximum size of the uploaded file in bytes. 0 = no maximum
	'max_size'			=> 0,

	// list of file extensions that a user is allowed to upload
	'ext_whitelist'		=> array(),

	// list of file extensions that a user is NOT allowed to upload
	'ext_blacklist'		=> array(),

	// list of file types that a user is allowed to upload
	// ( type is the part of the mime-type, before the slash )
	'type_whitelist'	=> array(),

	// list of file types that a user is NOT allowed to upload
	'type_blacklist'	=> array(),

	// list of file mime-types that a user is allowed to upload
	'mime_whitelist'	=> array(),

	// list of file mime-types that a user is NOT allowed to upload
	'mime_blacklist'	=> array(),

	/**
	 * file save settings
	*/

	// prefix given to every file when saved
	'prefix'			=> '',

	// suffix given to every file when saved
	'suffix'			=> '',

	// replace the extension of the uploaded file by this extension
	'extension'			=> '',

	// default path the uploaded files will be saved to
	'path'				=> '',

	// create the path if it doesn't exist
	'create_path'		=> true,

	// permissions to be set on the path after creation
	'path_chmod'		=> 0777,

	// permissions to be set on the uploaded file after being saved
	'file_chmod'		=> 0666,

	// if true, add a number suffix to the file if the file already exists
	'auto_rename'		=> true,

	// if true, overwrite the file if it already exists (only if auto_rename = false)
	'overwrite'			=> false,

	// if true, generate a random filename for the file being saved
	'randomize'			=> false,

	// if true, normalize the filename (convert to ASCII, replace spaces by underscores)
	'normalize'			=> false,

	// valid values are 'upper', 'lower', and false. case will be changed after all other transformations
	'change_case'		=> false,

	// maximum lengh of the filename, after all name modifications have been made. 0 = no maximum
	'max_length'		=> 0
);


