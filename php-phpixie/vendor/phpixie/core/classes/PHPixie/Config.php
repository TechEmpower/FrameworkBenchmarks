<?php

namespace PHPixie;

/**
 * Handles retrieving of the configuration options.
 * You can add configuration files to /assets/config folder
 * and later access them via the get() method.
 * @package Core
 */
class Config
{

	/**
	 * Pixie Dependancy Container
	 * @var \PHPixie\Pixie
	 */
	protected $pixie;
	
	/**
	 * Array of configuration files and values loaded from them
	 * @var array
	 */
	protected $groups = array();

	/**
	 * Constructs a config handler
	 *
	 * @param \PHPixie\Pixie $pixie Pixie dependency container
	 */
	public function __construct($pixie) {
		$this->pixie = $pixie;
	}
	
	/**
	 * Loads a group configuration file it has not been loaded before and
	 * returns its options. If the group doesn't exist creates an empty one
	 *
	 * @param string    $name Name of the configuration group to load
	 * @return array    Array of options for this group
	 */
	public function get_group($name)
	{

		if (!isset($this->groups[$name]))
		{
			$file = $this->pixie->find_file('config', $name);
			$this->load_group($name, $file);
		}

		return $this->groups[$name]['options'];
	}

	/**
	 * Loads group from file
	 *
	 * @param string $name Name to assign the loaded group
	 * @param string $file File to load
	 */
	public function load_group($name, $file)
	{
		
		if(!empty($file)){
			$options = include($file);
			if (!is_array($options))
				$options = array();
		}else {
			$options = array();
			$file = $this->pixie-> root_dir.'assets/config/'.$name.'.php';
		}
		
		$this->groups[$name] = array(
			'file' => $file,
			'options' => $options
		);
	}

	/**
	 * Retrieves a configuration value. You can use a dot notation
	 * to access properties in group arrays. The first part of the key
	 * specifies the configuration file from which options should be loaded from
	 * <code>
	 *     //Loads ['default']['user'] option
	 *     //from database.php configuration file
	 *     $this->pixie->config->get('database.default.user');
	 * </code>
	 *
	 * @param string    $key Configuration key to retrieve.
	 * @param string    $default Default value to return if the key is not found.
	 * @return mixed    Configuration value
	 * @throws \Exception If default value is not specified and the key is not found
	 */
	public function get()
	{
		$p = func_get_args();

		$keys = explode('.', $p[0]);
		$group_name = array_shift($keys);
		$group = $this->get_group($group_name);
		if (empty($keys))
			return $group;

		$total = count($keys);
		foreach ($keys as $i => $key)
		{
			if (isset($group[$key]))
			{
				if ($i == $total - 1)
					return $group[$key];
				$group = &$group[$key];
			}else
			{
				if (array_key_exists(1, $p))
					return $p[1];
				throw new \Exception("Configuration not set for {$p[0]}.");
			}
		}
	}

	/**
	 * Sets a configuration option.
	 *
	 * @param string    $key    Configuration key to set
	 * @param string    $value  Value to set for this option
	 */
	public function set($key, $value)
	{
		$keys = explode('.', $key);
		$group_name = array_shift($keys);
		$group = $this->get_group($group_name);
		$subgroup = &$group;
		$last_key = count($keys) - 1;
		foreach ($keys as $i => $key)
		{

			if ($i == $last_key)
			{

				$subgroup[$key] = $value;
			}
			else
			{

				if (!isset($subgroup[$key]) || !is_array($subgroup[$key])) 
					$subgroup[$key] = array();
				
				$subgroup = & $subgroup[$key];
			}
		}

		$this->groups[$group_name]['options'] = $group;
	}

	/**
	 * Writes a configuration group back to the file it was loaded from
	 *
	 * @param string    $group    Name of the group to write
	 */
	public function write($group)
	{
		$this->get_group($group);
		$group = $this->groups[$group];
		file_put_contents($group['file'], "<?php\r\nreturn ".var_export($group['options'], true).";");
	}

}
