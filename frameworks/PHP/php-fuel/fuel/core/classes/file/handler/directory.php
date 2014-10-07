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



class File_Handler_Directory
{

	/**
	 * @var	string	path to the file
	 */
	protected $path;

	/**
	 * @var	File_Area
	 */
	protected $area;

	/**
	 * @var	array	listing of files and directories within this directory
	 */
	protected $content = array();

	protected function __construct($path, array &$config, File_Area $area, $content = array())
	{
		$this->path	= rtrim($path, '\\/').DS;
		$this->area	= $area;

		foreach ($content as $key => $value)
		{
			if ( ! is_int($key))
			{
				$this->content[$key] = $value === false ? false : $area->get_handler($path.DS.$key, $config, $value);
			}
			else
			{
				$this->content[$key] = $area->get_handler($path.DS.$value, $config);
			}
		}
	}

	public static function forge($path, array $config = array(), File_Area $area = null, $content = array())
	{
		return new static($path, $config, $area, $content);
	}

	/**
	 * Read directory
	 *
	 * @param	$dept		whether or not to read recursive
	 * @param	$filters	whether or not to read recursive
	 * @return	array
	 */
	public function read($depth = 0, $filters = null)
	{
		return $this->area->read_dir($this->path, $depth, $filters, $this->area);
	}

	/**
	 * Rename file, only within current directory
	 *
	 * @param	string	new directory name
	 * @return	bool
	 */
	public function rename($new_name)
	{
		$info = pathinfo($this->path);

		$new_name = str_replace(array('..', '/', '\\'), array('', '', ''), $new_name);

		$new_path = $info['dirname'].DS.$new_name;

		$return =  $this->area->rename_dir($this->path, $new_path);
		$return and $this->path = $new_path;

		return $return;
	}

	/**
	 * Move directory to new parent directory
	 *
	 * @param	string	path to new parent directory, must be valid
	 * @return	bool
	 */
	public function move($new_path)
	{
		$info = pathinfo($this->path);
		$new_path = $this->area->get_path($new_path);

		$new_path = rtrim($new_path, '\\/').DS.$info['basename'];

		$return =  $this->area->rename_dir($this->path, $new_path);
		$return and $this->path = $new_path;

		return $return;
	}

	/**
	 * Copy directory
	 *
	 * @param	string	path to parent directory, must be valid
	 * @return	bool
	 */
	public function copy($new_path)
	{
		$info = pathinfo($this->path);
		$new_path = $this->area->get_path($new_path);

		$new_path = rtrim($new_path, '\\/').DS.$info['basename'];

		return $this->area->copy_dir($this->path, $new_path);
	}

	/**
	 * Update contents
	 *
	 * @param	mixed	new file contents
	 * @return	bool
	 */
	public function update()
	{
		throw new \BadMethodCallException('Update method is unavailable on directories.');
	}

	/**
	 * Delete directory
	 *
	 * @return	bool
	 */
	public function delete($recursive = true, $delete_top = true)
	{
		// should also destroy object but not possible in PHP right?
		return $this->area->delete_dir($this->path, $recursive, $delete_top);
	}

	/**
	 * Get the url.
	 *
	 * @return	bool
	 */
	public function get_url()
	{
		throw new \BadMethodCallException('Get_url method is unavailable on directories.');
	}

	/**
	 * Get the directory permissions.
	 *
	 * @return	string	file permissions
	 */
	public function get_permissions()
	{
		return $this->area->get_permissions($this->path);
	}

	/**
	 * Get directory's the created or modified timestamp.
	 *
	 * @param	string	$type	modified or created
	 * @return	int		Unix Timestamp
	 */
	public function get_time($type = 'modified')
	{
		return $this->area->get_time($this->path, $type);
	}

	/**
	 * Get the size.
	 *
	 * @return	bool
	 */
	public function get_size()
	{
		throw new \BadMethodCallException('Get_size method is unavailable on directories.');
	}
}


