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
 * The Asset class allows you to easily work with your apps assets.
 * It allows you to specify multiple paths to be searched for the
 * assets.
 *
 * You can configure the paths by copying the core/config/asset.php
 * config file into your app/config folder and changing the settings.
 *
 * @package     Fuel
 * @subpackage  Core
 */
class Asset_Instance
{

	/**
	 * @var  array  the asset paths to be searched
	 */
	protected $_asset_paths = array(
		'css' => array(),
		'js' => array(),
		'img' => array(),
	);

	/**
	 * @var  array  the sub-folders to be searched
	 */
	protected $_path_folders = array(
		'css' => 'css/',
		'js' => 'js/',
		'img' => 'img/',
	);

	/**
	 * @var  string  the URL to be prepended to all assets
	 */
	protected $_asset_url = '/';

	/**
	 * @var  bool  whether to append the file mtime to the url
	 */
	protected $_add_mtime = true;

	/**
	 * @var  array  holds the groups of assets
	 */
	protected $_groups = array();

	/**
	 * @var  string  prefix for generated output to provide proper indentation
	 */
	protected $_ident = '';

	/**
	 * @var  bool  if true, directly renders the output of no group name is given
	 */
	protected $_auto_render = true;

	/**
	 * @var  bool  if true the 'not found' exception will not be thrown and the asset is ignored.
	 */
	protected $_fail_silently = false;

	/**
	 * Parse the config and initialize the object instance
	 *
	 * @return  void
	 */
	public function __construct($config)
	{
		//global search path folders
		isset($config['css_dir']) and $this->_path_folders['css'] = $this->_unify_path($config['css_dir']);
		isset($config['js_dir']) and $this->_path_folders['js'] = $this->_unify_path($config['js_dir']);
		isset($config['img_dir']) and $this->_path_folders['img'] = $this->_unify_path($config['img_dir']);

		// global search paths
		foreach ($config['paths'] as $path)
		{
			$this->add_path($path);
		}

		// per-type search paths
		foreach ($config['folders'] as $type => $folders)
		{
			is_array($folders) or $folders = array($folders);

			foreach ($folders as $path)
			{
				$this->add_path($path, $type);
			}
		}

		$this->_add_mtime = $config['add_mtime'];
		$this->_asset_url = $config['url'];
		$this->_indent = str_repeat($config['indent_with'], $config['indent_level']);
		$this->_auto_render = $config['auto_render'];
		$this->_fail_silently = $config['fail_silently'];
	}

	/**
	 * Adds a new asset type to the list so we can load files of this type
	 *
	 * @param   string  new path type
	 * @param   string  optional default path
	 * @return  object  current instance
	 */
	public function add_type($type, $path = null)
	{
		isset($this->_asset_paths[$type]) or $this->_asset_paths[$type] = array();
		isset($this->_path_folders[$type]) or $this->_path_folders[$type] = $type.'/';

		if ( ! is_null($path))
		{
			$path = $this->_unify_path($path);
			$this->_asset_paths[$type][] = $path;
		}

		return $this;
	}

	/**
	 * Adds the given path to the front of the asset paths array.  It adds paths
	 * in a way so that asset paths are used First in Last Out.
	 *
	 * @param   string  the path to add
	 * @param   string  optional path type (js, css or img)
	 * @return  object  current instance
	 */
	public function add_path($path, $type = null)
	{
		is_null($type) and $type = $this->_path_folders;
		empty($path) and $path = DOCROOT;

		if( is_array($type))
		{
			foreach ($type as $key => $folder)
			{
				is_numeric($key) and $key = $folder;
				$folder = $this->_unify_path($path).ltrim($this->_unify_path($folder),DS);
				array_unshift($this->_asset_paths[$key], $folder);
			}
		}
		else
		{
			// create the asset type if it doesn't exist
			if ( ! isset($this->_asset_paths[$type]))
			{
				$this->_asset_paths[$type] = array();
				$this->_path_folders[$type] = $type.'/';
			}

			$path = $this->_unify_path($path);
			array_unshift($this->_asset_paths[$type], $path);
		}
		return $this;
	}

	/**
	 * Removes the given path from the asset paths array
	 *
	 * @param   string  the path to remove
	 * @param   string  optional path type (js, css or img)
	 * @return  object  current instance
	 */
	public function remove_path($path, $type = null)
	{
		is_null($type) and $type = $this->_path_folders;

		if( is_array($type))
		{
			foreach ($type as $key => $folder)
			{
				is_numeric($key) and $key = $folder;
				$folder = $this->_unify_path($path).ltrim($this->_unify_path($folder),DS);
				if (($found = array_search($folder, $this->_asset_paths[$key])) !== false)
				{
					unset($this->_asset_paths[$key][$found]);
				}
			}
		}
		else
		{
			$path = $this->_unify_path($path);
			if (($key = array_search($path, $this->_asset_paths[$type])) !== false)
			{
				unset($this->_asset_paths[$type][$key]);
			}
		}

		return $this;
	}

	/**
	 * Renders the given group.  Each tag will be separated by a line break.
	 * You can optionally tell it to render the files raw.  This means that
	 * all CSS and JS files in the group will be read and the contents included
	 * in the returning value.
	 *
	 * @param   mixed   the group to render
	 * @param   bool    whether to return the raw file or not
	 * @return  string  the group's output
	 */
	public function render($group = null, $raw = false)
	{
		is_null($group) and $group = '_default_';

		if (is_string($group))
		{
			isset($this->_groups[$group]) and $group = $this->_groups[$group];
		}

		is_array($group) or $group = array();

		$css = '';
		$js = '';
		$img = '';
		foreach ($group as $key => $item)
		{
			$type = $item['type'];
			$filename = $item['file'];
			$attr = $item['attr'];

			// only do a file search if the asset is not a URI
			if ( ! preg_match('|^(\w+:)?//|', $filename))
			{
				// and only if the asset is local to the applications base_url
				if ( ! preg_match('|^(\w+:)?//|', $this->_asset_url) or strpos($this->_asset_url, \Config::get('base_url')) === 0)
				{
					if ( ! ($file = $this->find_file($filename, $type)))
					{
						if ($this->_fail_silently)
						{
							continue;
						}

						throw new \FuelException('Could not find asset: '.$filename);
					}

					$raw or $file = $this->_asset_url.$file.($this->_add_mtime ? '?'.filemtime($file) : '');
				}
				else
				{
					$raw or $file = $this->_asset_url.$filename;
				}
			}
			else
			{
				$file = $filename;
			}

			switch($type)
			{
				case 'css':
					$attr['type'] = 'text/css';
					if ($raw)
					{
						$css .= html_tag('style', $attr, PHP_EOL.file_get_contents($file).PHP_EOL).PHP_EOL;
					}
					else
					{
						if ( ! isset($attr['rel']) or empty($attr['rel']))
						{
							$attr['rel'] = 'stylesheet';
						}
						$attr['href'] = $file;

						$css .= $this->_indent.html_tag('link', $attr).PHP_EOL;
					}
				break;
				case 'js':
					$attr['type'] = 'text/javascript';
					if ($raw)
					{
						$js .= html_tag('script', $attr, PHP_EOL.file_get_contents($file).PHP_EOL).PHP_EOL;
					}
					else
					{
						$attr['src'] = $file;

						$js .= $this->_indent.html_tag('script', $attr, '').PHP_EOL;
					}
				break;
				case 'img':
					$attr['src'] = $file;
					$attr['alt'] = isset($attr['alt']) ? $attr['alt'] : '';

					$img .= html_tag('img', $attr );
				break;
			}

		}

		// return them in the correct order
		return $css.$js.$img;
	}

	// --------------------------------------------------------------------

	/**
	 * CSS
	 *
	 * Either adds the stylesheet to the group, or returns the CSS tag.
	 *
	 * @access	public
	 * @param	mixed	       The file name, or an array files.
	 * @param	array	       An array of extra attributes
	 * @param	string	       The asset group name
	 * @return	string|object  Rendered asset or current instance when adding to group
	 */
	public function css($stylesheets = array(), $attr = array(), $group = null, $raw = false)
	{
		static $temp_group = 1000000;

		if ($group === null)
		{
			$render = $this->_auto_render;
			$group = $render ? (string) (++$temp_group) : '_default_';
		}
		else
		{
			$render = false;
		}

		$this->_parse_assets('css', $stylesheets, $attr, $group);

		if ($render)
		{
			return $this->render($group, $raw);
		}

		return $this;
	}

	// --------------------------------------------------------------------

	/**
	 * JS
	 *
	 * Either adds the javascript to the group, or returns the script tag.
	 *
	 * @access	public
	 * @param	mixed	       The file name, or an array files.
	 * @param	array	       An array of extra attributes
	 * @param	string	       The asset group name
	 * @return	string|object  Rendered asset or current instance when adding to group
	 */
	public function js($scripts = array(), $attr = array(), $group = null, $raw = false)
	{
		static $temp_group = 2000000;

		if ( ! isset($group))
		{
			$render = $this->_auto_render;
			$group = $render ? (string) (++$temp_group) : '_default_';
		}
		else
		{
			$render = false;
		}

		$this->_parse_assets('js', $scripts, $attr, $group);

		if ($render)
		{
			return $this->render($group, $raw);
		}

		return $this;
	}

	// --------------------------------------------------------------------

	/**
	 * Img
	 *
	 * Either adds the image to the group, or returns the image tag.
	 *
	 * @access	public
	 * @param	mixed	       The file name, or an array files.
	 * @param	array	       An array of extra attributes
	 * @param	string	       The asset group name
	 * @return	string|object  Rendered asset or current instance when adding to group
	 */
	public function img($images = array(), $attr = array(), $group = null)
	{
		static $temp_group = 3000000;

		if ( ! isset($group))
		{
			$render = $this->_auto_render;
			$group = $render ? (string) (++$temp_group) : '_default_';
		}
		else
		{
			$render = false;
		}

		$this->_parse_assets('img', $images, $attr, $group);

		if ($render)
		{
			return $this->render($group);
		}

		return $this;
	}

	// --------------------------------------------------------------------

	/**
	 * Find File
	 *
	 * Locates a file in all the asset paths.
	 *
	 * @access	public
	 * @param	string	The filename to locate
	 * @param	string	The sub-folder to look in (optional)
	 * @return	mixed	Either the path to the file or false if not found
	 */
	public function find_file($file, $type, $folder = '')
	{
		foreach ($this->_asset_paths[$type] as $path)
		{
			empty($folder) or $folder = $this->_unify_path($folder);

			if (is_file($newfile = $path.$folder.$this->_unify_path($file, null, false)))
			{
				strpos($newfile, DOCROOT) === 0 and $newfile = substr($newfile, strlen(DOCROOT));

				// return the file found, make sure it uses forward slashes on Windows
				return str_replace(DS, '/', $newfile);
			}
		}

		return false;
	}

	// --------------------------------------------------------------------

	/**
	 * Get File
	 *
	 * Locates a file in all the asset paths, and return it relative to the docroot
	 *
	 * @access	public
	 * @param	string	The filename to locate
	 * @param	string	The sub-folder to look in (optional)
	 * @return	mixed	Either the path to the file or false if not found
	 */
	public function get_file($file, $type, $folder = '')
	{
		if ($file = $this->find_file($file, $type, $folder))
		{
			return $this->_asset_url.$file;
		}

		return false;
	}

	// --------------------------------------------------------------------

	/**
	 * Parse Assets
	 *
	 * Pareses the assets and adds them to the group
	 *
	 * @access	private
	 * @param	string	The asset type
	 * @param	mixed	The file name, or an array files.
	 * @param	array	An array of extra attributes
	 * @param	string	The asset group name
	 * @return	string
	 */
	protected function _parse_assets($type, $assets, $attr, $group)
	{
		if ( ! is_array($assets))
		{
			$assets = array($assets);
		}

		foreach ($assets as $key => $asset)
		{
			// Prevent duplicate files in a group.
			if (\Arr::get($this->_groups, "$group.$key.file") == $asset)
			{
				continue;
			}

			$this->_groups[$group][] = array(
				'type'	=>	$type,
				'file'	=>	$asset,
				'attr'	=>	(array) $attr
			);
		}
	}

	// --------------------------------------------------------------------

	/**
	 * Unify the path
	 *
	 * make sure the directory separator in the path is correct for the
	 * platform used, is terminated with a directory separator, and all
	 * relative path references are removed
	 *
	 * @access	private
	 * @param	string	The path
	 * @param	mixed	Optional directory separator
	 * @return	string
	 */
	protected function _unify_path($path, $ds = null, $trailing = true)
	{
		$ds === null and $ds = DS;

		return rtrim(str_replace(array('\\', '/'), $ds, $path), $ds).($trailing ? $ds : '');
	}

}
