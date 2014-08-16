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

class ThemeException extends \FuelException {}

/**
 * Handles loading theme views and assets.
 */
class Theme
{
	/**
	 * All the Theme instances
	 *
	 * @var  array
	 */
	protected static $instances = array();

	/**
	 * Acts as a Multiton.  Will return the requested instance, or will create
	 * a new named one if it does not exist.
	 *
	 * @param   string    $name  The instance name
	 *
	 * @return  Theme
	 */
	public static function instance($name = '_default_', array $config = array())
	{
		if ( ! \array_key_exists($name, static::$instances))
		{
			static::$instances[$name] = static::forge($config);
		}

		return static::$instances[$name];
	}

	/**
	 * Gets a new instance of the Theme class.
	 *
	 * @param   array  $config  Optional config override
	 * @return  Theme
	 */
	public static function forge(array $config = array())
	{
		return new static($config);
	}

	/**
	 * @var  Asset_Instance  $asset  Asset instance for this theme instance
	 */
	public $asset = null;

	/**
	 * @var  array  $paths  Possible locations for themes
	 */
	protected $paths = array();

	/**
	 * @var  View  $template  View instance for this theme instance template
	 */
	public $template = null;

	/**
	 * @var  array  $active  Currently active theme
	 */
	protected $active = array(
		'name' => null,
		'path' => null,
		'asset_base' => false,
		'asset_path' => false,
		'info' => array(),
	);

	/**
	 * @var  array  $fallback  Fallback theme
	 */
	protected $fallback = array(
		'name' => null,
		'path' => null,
		'asset_base' => false,
		'asset_path' => false,
		'info' => array(),
	);

	/**
	 * @var  array  $config  Theme config
	 */
	protected $config = array(
		'active' => 'default',
		'fallback' => 'default',
		'paths' => array(),
		'assets_folder' => 'themes',
		'view_ext' => '.html',
		'require_info_file' => false,
		'info_file_name' => 'theme.info.php',
		'use_modules' => false,
	);

	/**
	 * @var  array  $partials	Storage for defined template partials
	 */
	protected $partials = array();

	/**
	 * @var  array  $chrome	Storage for defined partial chrome
	 */
	protected $chrome = array();

	/**
	 * Sets up the theme object.  If a config is given, it will not use the config
	 * file.
	 *
	 * @param   array  $config  Optional config override
	 * @return  void
	 */
	public function __construct(array $config = array())
	{
		if (empty($config))
		{
			\Config::load('theme', true, false, true);
			$config = \Config::get('theme', false);
		}

		// Order of this addition is important, do not change this.
		$this->config = $config + $this->config;

		// define the default theme paths...
		$this->add_paths($this->config['paths']);

		// create a unique asset instance for this theme instance...
		$this->asset = \Asset::forge('theme_'.spl_object_hash($this), array('paths' => array()));

		// and set the active and the fallback theme
		$this->active($this->config['active']);
		$this->fallback($this->config['fallback']);
	}

	/**
	 * Magic method, returns the output of [static::render].
	 *
	 * @return  string
	 * @uses    Theme::render
	 */
	public function __toString()
	{
		try
		{
			return (string) $this->render();
		}
		catch (\Exception $e)
		{
			\Error::exception_handler($e);

			return '';
		}
	}

	/**
	 * Sets the currently active theme.  Will return the currently active
	 * theme.  It will throw a \ThemeException if it cannot locate the theme.
	 *
	 * @param   string  $theme  Theme name to set active
	 * @return  array   The theme array
	 * @throws  \ThemeException
	 */
	public function active($theme = null)
	{
		return $this->set_theme($theme, 'active');
	}

	/**
	 * Sets the fallback theme.  This theme will be used if a view or asset
	 * cannot be found in the active theme.  Will return the fallback
	 * theme.  It will throw a \ThemeException if it cannot locate the theme.
	 *
	 * @param   string  $theme  Theme name to set active
	 * @return  array   The theme array
	 * @throws  \ThemeException
	 */
	public function fallback($theme = null)
	{
		return $this->set_theme($theme, 'fallback');
	}

	/**
	 * Loads a view from the currently active theme, the fallback theme, or
	 * via the standard FuelPHP cascading file system for views
	 *
	 * @param   string  $view         View name
	 * @param   array   $data         View data
	 * @param   bool    $auto_filter  Auto filter the view data
	 * @return  View    New View object
	 */
	public function view($view, $data = array(), $auto_filter = null)
	{
		if ($this->active['path'] === null)
		{
			throw new \ThemeException('You must set an active theme.');
		}

		return \View::forge($this->find_file($view), $data, $auto_filter);
	}

	/**
	 * Loads an asset from the currently loaded theme.
	 *
	 * @param   string  $path  Relative path to the asset
	 * @return  string  Full asset URL or path if outside docroot
	 */
	public function asset_path($path)
	{
		if ($this->active['path'] === null)
		{
			throw new \ThemeException('You must set an active theme.');
		}

		if ($this->active['asset_base'])
		{
			return $this->active['asset_base'].$path;
		}
		else
		{
			return $this->active['path'].$path;
		}
	}

	/**
	 * Sets a template for a theme
	 *
	 * @param   string  $template Name of the template view
	 * @return  View
	 */
	public function set_template($template)
	{
		// make sure the template is a View
		if (is_string($template))
		{
			$this->template = $this->view($template);
		}
		else
		{
			$this->template = $template;
		}

		// return the template view for chaining
		return $this->template;
	}

	/**
	 * Get the template view so it can be manipulated
	 *
	 * @return  string|View
	 * @throws  \ThemeException
	 */
	public function get_template()
	{
		// make sure the partial entry exists
		if (empty($this->template))
		{
			throw new \ThemeException('No valid template could be found. Use set_template() to define a page template.');
		}

		// return the template
		return $this->template;
	}

	/**
	 * Render the partials and the theme template
	 *
	 * @return  string|View
	 * @throws  \ThemeException
	 */
	public function render()
	{
		// make sure the template to be rendered is defined
		if (empty($this->template))
		{
			throw new \ThemeException('No valid template could be found. Use set_template() to define a page template.');
		}

		// pre-process all defined partials
		foreach ($this->partials as $key => $partials)
		{
			$output = '';
			foreach ($partials as $index => $partial)
			{
				// render the partial
				$output .= $partial->render();
			}

			// store the rendered output
			if ( ! empty($output) and array_key_exists($key, $this->chrome))
			{
				// encapsulate the partial in the chrome template
				$this->partials[$key] = $this->chrome[$key]['view']->set($this->chrome[$key]['var'], $output, false);
			}
			else
			{
				// store the partial output
				$this->partials[$key] = $output;
			}
		}

		// assign the partials to the template
		$this->template->set('partials', $this->partials, false);

		// return the template
		return $this->template;
	}

	/**
	 * Sets a partial for the current template
	 *
	 * @param   string  				$section   Name of the partial section in the template
	 * @param   string|View|ViewModel	$view      View, or name of the view
	 * @param   bool					$overwrite If true overwrite any already defined partials for this section
	 * @return  View
	 */
	public function set_partial($section, $view, $overwrite = false)
	{
		// make sure the partial entry exists
		array_key_exists($section, $this->partials) or $this->partials[$section] = array();

		// make sure the partial is a view
		if (is_string($view))
		{
			$name = $view;
			$view = $this->view($view);
		}
		else
		{
			$name = 'partial_'.count($this->partials[$section]);
		}

		// store the partial
		if ($overwrite)
		{
			$this->partials[$section] = array($name => $view);
		}
		else
		{
			$this->partials[$section][$name] = $view;
		}

		// return the partial view object for chaining
		return $this->partials[$section][$name];
	}

	/**
	 * Get a partial so it can be manipulated
	 *
	 * @param   string	$section   Name of the partial section in the template
	 * @param   string	$view      name of the view
	 * @return  View
	 * @throws  \ThemeException
	 */
	public function get_partial($section, $view)
	{
		// make sure the partial entry exists
		if ( ! array_key_exists($section, $this->partials) or ! array_key_exists($view, $this->partials[$section]))
		{
			throw new \ThemeException(sprintf('No partial named "%s" can be found in the "%s" section.', $view, $section));
		}

		return $this->partials[$section][$view];
	}

	/**
	 * Sets a chrome for a partial
	 *
	 * @param   string  				$section	Name of the partial section in the template
	 * @param   string|View|ViewModel	$view   	chrome View, or name of the view
	 * @param   string  				$var		Name of the variable in the chome that will output the partial
	 *
	 * @return  void
	 */
	public function set_chrome($section, $view, $var = 'content')
	{
		// make sure the chrome is a view
		if (is_string($view))
		{
			$view = $this->view($view);
		}

		$this->chrome[$section] = array('var' => $var, 'view' => $view);
	}

	/**
	 * Get a set chrome view
	 *
	 * @param   string  				$section	Name of the partial section in the template
	 * @param   string|View|ViewModel	$view   	chrome View, or name of the view
	 * @param   string  				$var		Name of the variable in the chome that will output the partial
	 *
	 * @return  void
	 */
	public function get_chrome($section)
	{
		// make sure the partial entry exists
		if ( ! array_key_exists($section, $this->chrome))
		{
			throw new \ThemeException(sprintf('No chrome for a partial named "%s" can be found.', $section));
		}

		return $this->chrome[$section]['view'];
	}

	/**
	 * Adds the given path to the theme search path.
	 *
	 * @param   string  $path  Path to add
	 * @return  void
	 */
	public function add_path($path)
	{
		$this->paths[] = rtrim($path, DS).DS;
	}

	/**
	 * Adds the given paths to the theme search path.
	 *
	 * @param   array  $paths  Paths to add
	 * @return  void
	 */
	public function add_paths(array $paths)
	{
		array_walk($paths, array($this, 'add_path'));
	}

	/**
	 * Finds the given theme by searching through all of the theme paths.  If
	 * found it will return the path, else it will return `false`.
	 *
	 * @param   string  $theme  Theme to find
	 * @return  string|false  Path or false if not found
	 */
	public function find($theme)
	{
		foreach ($this->paths as $path)
		{
			if (is_dir($path.$theme))
			{
				return $path.$theme.DS;
			}
		}

		return false;
	}

	/**
	 * Gets an array of all themes in all theme paths, sorted alphabetically.
	 *
	 * @return  array
	 */
	public function all()
	{
		$themes = array();
		foreach ($this->paths as $path)
		{
			foreach(glob($path.'*', GLOB_ONLYDIR) as $theme)
			{
				$themes[] = basename($theme);
			}
		}
		sort($themes);

		return $themes;
	}

	/**
	 * Get a value from the info array
	 *
	 * @return  mixed
	 */
	public function get_info($var = null, $default = null, $theme = null)
	{
		// if no theme is given
		if ($theme === null)
		{
			// if no var to search is given return the entire active info array
			if ($var === null)
			{
				return $this->active['info'];
			}

			// find the value in the active theme info
			if (($value = \Arr::get($this->active['info'], $var, null)) !== null)
			{
				return $value;
			}

			// and if not found, check the fallback
			elseif (($value = \Arr::get($this->fallback['info'], $var, null)) !== null)
			{
				return $value;
			}
		}

		// or if we have a specific theme
		else
		{
			// fetch the info from that theme
			$info = $this->load_info($theme);

			// and return the requested value
			return $var === null ? $info : \Arr::get($info, $var, $default);
		}

		// not found, return the given default value
		return $default;
	}

	/**
	 * Set a value in the info array
	 *
	 * @return  Theme
	 */
	public function set_info($var, $value = null, $type = 'active')
	{
		if ($type == 'active')
		{
			\Arr::set($this->active['info'], $var, $value);
		}
		elseif ($type == 'fallback')
		{
			\Arr::set($this->fallback['info'], $var, $value);
		}

		// return for chaining
		return $this;
	}

	/**
	 * Load in the theme.info file for the given (or active) theme.
	 *
	 * @param   string  $theme  Name of the theme (null for active)
	 * @return  array   Theme info array
	 */
	public function load_info($theme = null)
	{
		if ($theme === null)
		{
			$theme = $this->active;
		}

		if (is_array($theme))
		{
			$path = $theme['path'];
			$name = $theme['name'];
		}
		else
		{
			$path = $this->find($theme);
			$name = $theme;
			$theme = array(
				'name' => $name,
				'path' => $path
			);
		}

		if ( ! $path)
		{
			throw new \ThemeException(sprintf('Could not find theme "%s".', $theme));
		}

		if (($file = $this->find_file($this->config['info_file_name'], array($theme))) == $this->config['info_file_name'])
		{
			if ($this->config['require_info_file'])
			{
				throw new \ThemeException(sprintf('Theme "%s" is missing "%s".', $name, $this->config['info_file_name']));
			}
			else
			{
				return array();
			}
		}

		return \Config::load($file, false, true);
	}

	/**
	 * Save the theme.info file for the active (or fallback) theme.
	 *
	 * @param   string  $type  Name of the theme (null for active)
	 * @return  array   Theme info array
	 */
	public function save_info($type = 'active')
	{
		if ($type == 'active')
		{
			$theme = $this->active;
		}
		elseif ($type == 'fallback')
		{
			$theme = $this->fallback;
		}
		else
		{
			throw new \ThemeException('No location found to save the info file to.');
		}

		if ( ! $theme['path'])
		{
			throw new \ThemeException(sprintf('Could not find theme "%s".', $theme['name']));
		}

		if ( ! ($file = $this->find_file($this->config['info_file_name'], array($theme['name']))))
		{
			throw new \ThemeException(sprintf('Theme "%s" is missing "%s".', $theme['name'], $this->config['info_file_name']));
		}

		return \Config::save($file, $theme['info']);
	}

	/**
	 * Enable or disable the use of modules. If enabled, every theme view loaded
	 * will be prefixed with the module name, so you don't have to hardcode the
	 * module name as a view file prefix
	 *
	 * @param	$enable	enable if true, disable if false
	 * @return	Theme
	 */
	public function use_modules($enable = true)
	{
		$this->config['use_modules'] = (bool) $enable;

		// return for chaining
		return $this;
	}

	/**
	 * Find the absolute path to a file in a set of Themes.  You can optionally
	 * send an array of themes to search.  If you do not, it will search active
	 * then fallback (in that order).
	 *
	 * @param   string  $view    name of the view to find
	 * @param   array   $themes  optional array of themes to search
	 * @return  string  absolute path to the view
	 * @throws  \ThemeException  when not found
	 */
	protected function find_file($view, $themes = null)
	{
		if ($themes === null)
		{
			$themes = array($this->active, $this->fallback);
		}

		// determine the path prefix
		$path_prefix = '';
		if ($this->config['use_modules'] and $module = \Request::active()->module)
		{
			$path_prefix = $module.DS;
		}

		foreach ($themes as $theme)
		{
			$ext   = pathinfo($view, PATHINFO_EXTENSION) ?
				'.'.pathinfo($view, PATHINFO_EXTENSION) : $this->config['view_ext'];
			$file  = (pathinfo($view, PATHINFO_DIRNAME) ?
					str_replace(array('/', DS), DS, pathinfo($view, PATHINFO_DIRNAME)).DS : '').
				pathinfo($view, PATHINFO_FILENAME);
			if (empty($theme['find_file']))
			{
				if (is_file($path = $theme['path'].$path_prefix.$file.$ext))
				{
					return $path;
				}
				elseif (is_file($path = $theme['path'].$file.$ext))
				{
					return $path;
				}
			}
			else
			{
				if ($path = \Finder::search($theme['path'].$path_prefix, $file, $ext))
				{
					return $path;
				}
			}
		}

		// not found, return the viewname to fall back to the standard View processing
		return $view;
	}

	/**
	 * Sets a  theme.
	 *
	 * @param   string  $theme  Theme name to set active
	 * @param   string  $type   name of the internal theme array to set
	 * @return  array   The theme array
	 * @throws  \ThemeException
	 */
	protected function set_theme($theme = null, $type = 'active')
	{
		// remove the defined theme asset paths from the asset instance
		empty($this->active['asset_path']) or $this->asset->remove_path($this->active['asset_path']);
		empty($this->fallback['asset_path']) or $this->asset->remove_path($this->fallback['asset_path']);

		// set the fallback theme
		if ($theme !== null)
		{
			$this->{$type} = $this->create_theme_array($theme);
		}

		// add the asset paths to the asset instance
		empty($this->fallback['asset_path']) or $this->asset->add_path($this->fallback['asset_path']);
		empty($this->active['asset_path']) or $this->asset->add_path($this->active['asset_path']);

		return $this->{$type};
	}

	/**
	 * Creates a theme array by locating the given theme and setting all of the
	 * option.  It will throw a \ThemeException if it cannot locate the theme.
	 *
	 * @param   string  $theme  Theme name to set active
	 * @return  array   The theme array
	 * @throws  \ThemeException
	 */
	protected function create_theme_array($theme)
	{
		if ( ! is_array($theme))
		{
			if ( ! $path = $this->find($theme))
			{
				throw new \ThemeException(sprintf('Theme "%s" could not be found.', $theme));
			}

			$theme = array(
				'name' => $theme,
				'path' => $path,
			);
		}
		else
		{
			if ( ! isset($theme['name']) or ! isset($theme['path']))
			{
				throw new \ThemeException('Theme name and path must be given in array config.');
			}
		}

		// load the theme info file
		if ( ! isset($theme['info']))
		{
			$theme['info'] = $this->load_info($theme);
		}

		if ( ! isset($theme['asset_base']))
		{
			// determine the asset location and base URL
			$assets_folder = rtrim($this->config['assets_folder'], DS).'/';

			// all theme files are inside the docroot
			if (strpos($path, DOCROOT) === 0 and is_dir($path.$assets_folder))
			{
				$theme['asset_path'] = $path.$assets_folder;
				$theme['asset_base'] = str_replace(DOCROOT, '', $theme['asset_path']);
			}

			// theme views and templates are outside the docroot
			else
			{
				$theme['asset_base'] = $assets_folder.$theme['name'].'/';
			}
		}

		if ( ! isset($theme['asset_path']) and strpos($theme['asset_base'], '://') === false)
		{
			$theme['asset_path'] = DOCROOT.$theme['asset_base'];
		}

		// always uses forward slashes (DS is a backslash on Windows)
		$theme['asset_base'] = str_replace(DS, '/', $theme['asset_base']);

		return $theme;
	}
}
