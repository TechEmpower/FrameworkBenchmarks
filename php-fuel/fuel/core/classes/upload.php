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

namespace Fuel\Core;

/**
 * Upload Class
 *
 * @package		Fuel
 * @category	Core
 * @author		Harro "WanWizard" Verton
 * @link		http://docs.fuelphp.com/classes/upload.html
 */
class Upload
{

	/* ---------------------------------------------------------------------------
	 * ERROR CODE CONSTANTS
	 * --------------------------------------------------------------------------- */

	// duplicate the PHP standard error codes for consistency
	const UPLOAD_ERR_OK         = UPLOAD_ERR_OK;
	const UPLOAD_ERR_INI_SIZE   = UPLOAD_ERR_INI_SIZE;
	const UPLOAD_ERR_FORM_SIZE  = UPLOAD_ERR_FORM_SIZE;
	const UPLOAD_ERR_PARTIAL    = UPLOAD_ERR_PARTIAL;
	const UPLOAD_ERR_NO_FILE    = UPLOAD_ERR_NO_FILE;
	const UPLOAD_ERR_NO_TMP_DIR = UPLOAD_ERR_NO_TMP_DIR;
	const UPLOAD_ERR_CANT_WRITE = UPLOAD_ERR_CANT_WRITE;
	const UPLOAD_ERR_EXTENSION  = UPLOAD_ERR_EXTENSION;

	// and add our own error codes
	const UPLOAD_ERR_MAX_SIZE             = 101;
	const UPLOAD_ERR_EXT_BLACKLISTED      = 102;
	const UPLOAD_ERR_EXT_NOT_WHITELISTED  = 103;
	const UPLOAD_ERR_TYPE_BLACKLISTED     = 104;
	const UPLOAD_ERR_TYPE_NOT_WHITELISTED = 105;
	const UPLOAD_ERR_MIME_BLACKLISTED     = 106;
	const UPLOAD_ERR_MIME_NOT_WHITELISTED = 107;
	const UPLOAD_ERR_MAX_FILENAME_LENGTH  = 108;
	const UPLOAD_ERR_MOVE_FAILED          = 109;
	const UPLOAD_ERR_DUPLICATE_FILE       = 110;
	const UPLOAD_ERR_MKDIR_FAILED         = 111;
	const UPLOAD_ERR_FTP_FAILED           = 112;

	/* ---------------------------------------------------------------------------
	 * STATIC PROPERTIES
	 * --------------------------------------------------------------------------- */

	/**
	 * @var array default configuration values
	 */
	protected static $_defaults = array(
		'auto_process'    => false,
		// validation settings
		'max_size'        => 0,
		'max_length'      => 0,
		'ext_whitelist'   => array(),
		'ext_blacklist'   => array(),
		'type_whitelist'  => array(),
		'type_blacklist'  => array(),
		'mime_whitelist'  => array(),
		'mime_blacklist'  => array(),
		// save settings
		'path'            => '',
		'prefix'          => '',
		'suffix'          => '',
		'extension'       => '',
		'create_path'     => true,
		'path_chmod'      => 0777,
		'file_chmod'      => 0666,
		'auto_rename'     => true,
		'overwrite'       => false,
		'randomize'       => false,
		'normalize'       => false,
		'normalize_separator' => '_',
		'change_case'     => false,
		'ftp_mode'        => 'auto',
		'ftp_permissions' => null
	);

	/**
	 * @var array defined callbacks
	 */
	protected static $callbacks = array(
		'validate' => null,
		'before'   => null,
		'after'    => null
	);

	/**
	 * @var array configuration of this instance
	 */
	protected static $config = array();

	/**
	 * @var array normalized $_FILES array
	 */
	protected static $files = array();

	/**
	 * @var bool indicator of valid uploads
	 */
	protected static $valid = false;

	/**
	 * @var object Ftp object
	 */
	protected static $with_ftp = false;

	/* ---------------------------------------------------------------------------
	 * STATIC METHODS
	 * --------------------------------------------------------------------------- */

	/**
	 * class initialisation, load the config and process $_FILES if needed
	 *
	 * @return	void
	 */
	public static function _init()
	{
		// get the config for this upload
		\Config::load('upload', true);

		// get the language file for this upload
		\Lang::load('upload', true);

		// make sure we have defaults for those not defined
		static::$config = array_merge(static::$_defaults, \Config::get('upload', array()));

		static::$config['auto_process'] and static::process();
	}

	// ---------------------------------------------------------------------------

	/**
	 * Check if we have valid files
	 *
	 * @return	bool	true if static:$files contains uploaded files that are valid
	 */
	public static function is_valid()
	{
		return static::$valid;
	}

	// ---------------------------------------------------------------------------

	/**
	 * Get the list of validated files
	 *
	 * @return	array	list of uploaded files that are validated
	 */
	public static function get_files($index = null)
	{
		// if no parameters were passed, return all files successfully uploaded
		if (func_num_args() == 0)
		{
			return array_filter(static::$files, function($file) { return $file['error'] === false; } );
		}

		// if an index number was given, return that file only (if that was successful)
		elseif (isset(static::$files[$index]) and static::$files[$index]['error'] === false)
		{
			return static::$files[$index];
		}

		// if an field name was given, return that file only (if that was successful)
		else
		{
			foreach (static::$files as $file)
			{
				if ($file['field'] == $index and $file['error'] === false)
				{
					return $file;
				}
			}

			throw new \FuelException('No valid uploaded file exists with index "'.$index.'"');
		}
	}

	// ---------------------------------------------------------------------------

	/**
	 * Get the list of non-validated files
	 *
	 * @return	array	list of uploaded files that failed to validate
	 */
	public static function get_errors($index = null)
	{
		// if no parameters were passed, return all files in error
		if (func_num_args() == 0)
		{
			return array_filter(static::$files, function($file) { return $file['error'] === true; } );
		}

		// if an index number was given, return that file only (if it is in error)
		elseif (isset(static::$files[$index]) and static::$files[$index]['error'] === true)
		{
			return static::$files[$index];
		}

		// if an field name was given, return that file only (if it is in error)
		else
		{
			foreach (static::$files as $file)
			{
				if ($file['field'] == $index and $file['error'] === true)
				{
					return $file;
				}
			}

			throw new \FuelException('No invalid uploaded file exists with index "'.$index.'"');
		}
	}

	// --------------------------------------------------------------------

	/**
	 * Register
	 *
	 * Registers a Callback for a given event
	 *
	 * @access	public
	 * @param	string	The name of the event
	 * @param	mixed	callback information
	 * @return	void
	 */
	public static function register()
	{
		// get any arguments passed
		$callback = func_get_args();

		// if the arguments are valid, register the callback
		if (isset($callback[0]) and is_string($callback[0]) and isset($callback[1]) and is_callable($callback[1]))
		{
			// make sure we have an entry for this callback
			if (array_key_exists($callback[0], static::$callbacks))
			{
				static::$callbacks[array_shift($callback)] = $callback;

				// report success
				return true;
			}
		}

		// can't register the callback
		return false;
	}

	// ---------------------------------------------------------------------------

	/**
	 * Normalize the $_FILES array and store the result in $files
	 *
	 * @return	void
	 */
	public static function process($config = array())
	{
		// process runtime config
		if (is_array($config))
		{
			static::$config = array_merge(static::$config, $config);
		}

		// processed files array's
		static::$files = array();
		$files = array();

		// any files uploaded?
		if ( empty($_FILES))
		{
			throw new \FuelException('No file upload was initiated. Did you specify "enctype" in your &lt;form&gt; tag?');
		}

		// normalize the multidimensional fields in the $_FILES array
		foreach($_FILES as $name => $value)
		{
			if (is_array($value['name']))
			{
				foreach($value as $field => $content)
				{
					foreach(\Arr::flatten($content) as $element => $data)
					{
						$_FILES[$name.':'.$element][$field] = $data;
					}
				}
				unset($_FILES[$name]);
			}
		}

		// normalize the $_FILES array
		foreach($_FILES as $name => $value)
		{
			// store the file data
			$file = array('field' => $name, 'file' => $value['tmp_name']);
			if ($value['error'])
			{
				$file['error'] = true;
				$file['errors'][] = array('error' => $value['error']);
			}
			else
			{
				$file['error'] = false;
				$file['errors'] = array();
			}
			unset($value['tmp_name']);
			$files[] = array_merge($value, $file);
		}

		// verify and augment the files data
		foreach($files as $key => $value)
		{
			// add some filename details (pathinfo can't be trusted with utf-8 filenames!)
			$files[$key]['extension'] = ltrim(strrchr(ltrim($files[$key]['name'], '.'), '.'),'.');
			if (empty($files[$key]['extension']))
			{
				$files[$key]['filename'] = $files[$key]['name'];
			}
			else
			{
				$files[$key]['filename'] = substr($files[$key]['name'], 0, strlen($files[$key]['name'])-(strlen($files[$key]['extension'])+1));
			}

			// does this upload exceed the maximum size?
			if (! empty(static::$config['max_size']) and $files[$key]['size'] > static::$config['max_size'])
			{
				$files[$key]['error'] = true;
				$files[$key]['errors'][] = array('error' => static::UPLOAD_ERR_MAX_SIZE);
			}

			// add mimetype information
			if ( ! $files[$key]['error'])
			{
				$handle = finfo_open(FILEINFO_MIME_TYPE);
				$files[$key]['mimetype'] = finfo_file($handle, $value['file']);
				finfo_close($handle);
				if ($files[$key]['mimetype'] == 'application/octet-stream' and $files[$key]['type'] != $files[$key]['mimetype'])
				{
					$files[$key]['mimetype'] = $files[$key]['type'];
				}

				// make sure it contains something valid
				if (empty($files[$key]['mimetype']))
				{
					$files[$key]['mimetype'] = 'application/octet-stream';
				}
			}

			// check the file extension black- and whitelists
			if ( ! $files[$key]['error'])
			{
				if (in_array(strtolower($files[$key]['extension']), (array) static::$config['ext_blacklist']))
				{
					$files[$key]['error'] = true;
					$files[$key]['errors'][] = array('error' => static::UPLOAD_ERR_EXT_BLACKLISTED);
				}
				elseif ( ! empty(static::$config['ext_whitelist']) and ! in_array(strtolower($files[$key]['extension']), (array) static::$config['ext_whitelist']))
				{
					$files[$key]['error'] = true;
					$files[$key]['errors'][] = array('error' => static::UPLOAD_ERR_EXT_NOT_WHITELISTED);
				}
			}

			// check the file type black- and whitelists
			if ( ! $files[$key]['error'])
			{
				// split the mimetype info so we can run some tests
				preg_match('|^(.*)/(.*)|', $files[$key]['mimetype'], $mimeinfo);

				if (in_array($mimeinfo[1], (array) static::$config['type_blacklist']))
				{
					$files[$key]['error'] = true;
					$files[$key]['errors'][] = array('error' => static::UPLOAD_ERR_TYPE_BLACKLISTED);
				}
				if ( ! empty(static::$config['type_whitelist']) and ! in_array($mimeinfo[1], (array) static::$config['type_whitelist']))
				{
					$files[$key]['error'] = true;
					$files[$key]['errors'][] = array('error' => static::UPLOAD_ERR_TYPE_NOT_WHITELISTED);
				}
			}

			// check the file mimetype black- and whitelists
			if ( ! $files[$key]['error'])
			{
				if (in_array($files[$key]['mimetype'], (array) static::$config['mime_blacklist']))
				{
					$files[$key]['error'] = true;
					$files[$key]['errors'][] = array('error' => static::UPLOAD_ERR_MIME_BLACKLISTED);
				}
				elseif ( ! empty(static::$config['mime_whitelist']) and ! in_array($files[$key]['mimetype'], (array) static::$config['mime_whitelist']))
				{
					$files[$key]['error'] = true;
					$files[$key]['errors'][] = array('error' => static::UPLOAD_ERR_MIME_NOT_WHITELISTED);
				}
			}

			// store the normalized and validated result
			static::$files[$key] = $files[$key];

			// validation callback defined?
			if (array_key_exists('validate', static::$callbacks) and ! is_null(static::$callbacks['validate']))
			{
				// get the callback method
				$callback = static::$callbacks['validate'][0];

				// call the callback
				if (is_callable($callback))
				{
					$result = call_user_func_array($callback, array(&static::$files[$key]));
					if (is_numeric($result) and $result)
					{
						static::$files[$key]['error'] = true;
						static::$files[$key]['errors'][] = array('error' => $result);
					}
				}
			}
			// and add the message texts
			foreach (static::$files[$key]['errors'] as $e => $error)
			{
				empty(static::$files[$key]['errors'][$e]['message']) and static::$files[$key]['errors'][$e]['message'] = \Lang::get('upload.error_'.$error['error']);
			}
		}

		// determine the validate status of at least one uploaded file
		$valid = false;
		foreach(static::$files as $key => $value)
		{
			if ($value['error'] === false)
			{
				$valid = true;
				break;
			}
		}
		static::$valid = $valid;
	}

	// ---------------------------------------------------------------------------

	/**
	 * Upload files with Ftp
	 *
	 * @param   string|array  The name of the config group to use, or a configuration array.
	 * @param   bool          Automatically connect to this server.
	 */
	public static function with_ftp($config = 'default', $connect = true)
	{
		static::$with_ftp = \Ftp::forge($config, $connect);
	}

	// ---------------------------------------------------------------------------

	/**
	 * save uploaded file(s)
	 *
	 * @param	mixed	if int, $files element to move. if array, list of elements to move, if none, move all elements
	 * @param	string	path to move to
	 * @return	void
	 */
	public static function save()
	{
		// path to save the files to
		$path = static::$config['path'];

		// files to save
		$files = array();

		// check for parameters
		if (func_num_args())
		{
			foreach(func_get_args() as $param)
			{
				// string => new path to save to
				if (is_string($param))
				{
					$path = $param;
				}
				// array => list of $files indexes to save
				elseif(is_array($param))
				{
					$files = array();
					foreach($param as $key)
					{
						if (isset(static::$files[(int) $key]))
						{
							$files[(int) $key] = static::$files[(int) $key];
						}
					}
				}
				// integer => files index to save
				elseif(is_numeric($param))
				{
					if (isset(static::$files[$param]))
					{
						$files[$param] = static::$files[$param];
					}
				}
			}
		}
		else
		{
			// save all files
			$files = static::$files;
		}

		// anything to save?
		if (empty($files))
		{
			throw new \FuelException('No uploaded files are selected.');
		}

		// supplied new name and not auto renaming?
		if (array_key_exists('new_name', static::$config) and ! static::$config['auto_rename'] and count($files) > 1)
		{
			throw new \FuelException('Can\'t rename multiple files without auto renaming.');
		}

		// make sure we have a valid path
		$path = rtrim($path, DS).DS;
		if ( ! is_dir($path) and (bool) static::$config['create_path'])
		{
			$oldumask = umask(0);
			@mkdir($path, static::$config['path_chmod'], true);
			umask($oldumask);
		}

		if ( ! is_dir($path))
		{
			throw new \FuelException('Can\'t move the uploaded file. Destination path specified does not exist.');
		}

		// save the old umask
		$oldumask = umask(0);

		// now that we have a path, let's save the files
		foreach($files as $key => $file)
		{
			// skip all files in error
			if ($file['error'] === true)
			{
				continue;
			}

			// do we need to generate a random filename?
			if ( (bool) static::$config['randomize'])
			{
				$filename = md5(serialize($file));
			}
			else
			{
				$filename  = $file['filename'];
				if ( (bool) static::$config['normalize'])
				{
					$filename = \Inflector::friendly_title($filename, static::$config['normalize_separator']);
				}
			}

			array_key_exists('new_name', static::$config) and $filename = (string) static::$config['new_name'];

			// array with the final filename
			$save_as = array(
				static::$config['prefix'],
				$filename,
				static::$config['suffix'],
				'',
				'.',
				empty(static::$config['extension']) ? $file['extension'] : static::$config['extension']
			);
			// remove the dot if no extension is present
			if (empty($save_as[5]))
			{
				$save_as[4] = '';
			}

			// need to modify case?
			switch(static::$config['change_case'])
			{
				case 'upper':
					$save_as = array_map(function($var) { return strtoupper($var); }, $save_as);
				break;

				case 'lower':
					$save_as = array_map(function($var) { return strtolower($var); }, $save_as);
				break;

				default:
				break;
			}


			// check if the file already exists
			if (file_exists($path.implode('', $save_as)))
			{
				if ( (bool) static::$config['auto_rename'])
				{
					$counter = 0;
					do
					{
						$save_as[3] = '_'.++$counter;
					}
					while (file_exists($path.implode('', $save_as)));
				}
				else
				{
					if ( ! (bool) static::$config['overwrite'])
					{
						static::$files[$key]['error'] = true;
						static::$files[$key]['errors'][] = array('error' => static::UPLOAD_ERR_DUPLICATE_FILE);
						continue;
					}
				}
			}

			// no need to store it as an array anymore
			$save_as = implode('', $save_as);

			// does the filename exceed the maximum length?
			if ( ! empty(static::$config['max_length']) and strlen($save_as) > static::$config['max_length'])
			{
				static::$files[$key]['error'] = true;
				static::$files[$key]['errors'][] = array('error' => static::UPLOAD_ERR_MAX_FILENAME_LENGTH);
				continue;
			}

			// if no error was detected, move the file
			if ( ! static::$files[$key]['error'])
			{
				// save the additional information
				static::$files[$key]['saved_to'] = $path;
				static::$files[$key]['saved_as'] = $save_as;

				// before callback defined?
				if (array_key_exists('before', static::$callbacks) and ! is_null(static::$callbacks['before']))
				{
					// get the callback method
					$callback = static::$callbacks['before'][0];

					// call the callback
					if (is_callable($callback))
					{
						$result = call_user_func_array($callback, array(&static::$files[$key]));
						if (is_numeric($result))
						{
							static::$files[$key]['error'] = true;
							static::$files[$key]['errors'][] = array('error' => $result);
						}
					}

					// recheck the saved_to path, it might have been altered
					if ( ! is_dir(static::$files[$key]['saved_to']) and (bool) static::$config['create_path'])
					{
						@mkdir(static::$files[$key]['saved_to'], static::$config['path_chmod'], true);
					}
					if ( ! is_dir(static::$files[$key]['saved_to']))
					{
						static::$files[$key]['error'] = true;
						static::$files[$key]['errors'][] = array('error' => static::UPLOAD_ERR_MKDIR_FAILED);
						continue;
					}
				}

				// move the uploaded file
				if ( ! static::$files[$key]['error'])
				{
					// check if file should be uploaded with ftp
					if (static::$with_ftp)
					{
						$uploaded = static::$with_ftp->upload($file['file'], static::$config['path'].static::$files[$key]['saved_as'], static::$config['ftp_mode'], static::$config['ftp_permissions']);

						if ( ! $uploaded)
						{
							static::$files[$key]['error'] = true;
							static::$files[$key]['errors'][] = array('error' => static::UPLOAD_ERR_FTP_FAILED);
						}
					}
					else
					{
						if( ! @move_uploaded_file($file['file'], static::$files[$key]['saved_to'].static::$files[$key]['saved_as']) )
						{
							static::$files[$key]['error'] = true;
							static::$files[$key]['errors'][] = array('error' => static::UPLOAD_ERR_MOVE_FAILED);
						}
						else
						{
							@chmod(static::$files[$key]['saved_to'].static::$files[$key]['saved_as'], static::$config['file_chmod']);
						}

						// after callback defined?
						if (array_key_exists('after', static::$callbacks) and ! is_null(static::$callbacks['after']))
						{
							// get the callback method
							$callback = static::$callbacks['after'][0];

							// call the callback
							if (is_callable($callback))
							{
								$result = call_user_func_array($callback, array(&static::$files[$key]));
								if (is_numeric($result))
								{
									static::$files[$key]['error'] = true;
									static::$files[$key]['errors'][] = array('error' => $result);
								}
							}
						}
					}
				}
			}
		}

		// and add the message texts
		foreach (static::$files[$key]['errors'] as $e => $error)
		{
			empty(static::$files[$key]['errors'][$e]['message']) and static::$files[$key]['errors'][$e]['message'] = \Lang::get('upload.error_'.$error['error']);
		}

		// reset the umask
		umask($oldumask);
	}

}


