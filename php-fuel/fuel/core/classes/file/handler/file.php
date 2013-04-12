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



class File_Handler_File
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
	 * @var	Resource	file resource
	 */
	protected $resource;

	/**
	 * @var	bool	whether the current object is read only
	 */
	protected $readonly = false;

	protected function __construct($path, array $config, File_Area $area, $content = array())
	{
		$this->path = $path;
		$this->area = $area;
	}

	public static function forge($path, array $config = array(), File_Area $area = null, $content = array())
	{
		$obj = new static($path, $config, \File::instance($area), $content);

		$config['path'] = $path;
		$config['area'] = $area;
		foreach ($config as $key => $value)
		{
			if (property_exists($obj, $key) && empty($obj->$key))
			{
				$obj->$key = $value;
			}
		}

		return $obj;
	}

	/**
	 * Read file
	 *
	 * @param	bool	whether to use file_get_contents() or readfile()
	 * @return	string|IO
	 */
	public function read($as_string = false)
	{
		return $this->area->read($this->path, $as_string);
	}

	/**
	 * Rename file, only within current directory
	 *
	 * @param	string			new filename
	 * @param	string|false	new extension, false to keep current
	 * @return	bool
	 */
	public function rename($new_name, $new_extension = false)
	{
		$info = pathinfo($this->path);

		$new_name = str_replace(array('..', '/', '\\'), array('', '', ''), $new_name);
		$extension = $new_extension === false
			? $info['extension']
			: ltrim($new_extension, '.');
		$extension = ! empty($extension) ? '.'.$extension : '';

		$new_path = $info['dirname'].DS.$new_name.$extension;

		$return =  $this->area->rename($this->path, $new_path);
		$return and $this->path = $new_path;

		return $return;
	}

	/**
	 * Move file to new directory
	 *
	 * @param	string	path to new directory, must be valid
	 * @return	bool
	 */
	public function move($new_path)
	{
		$info = pathinfo($this->path);

		$new_path = $this->area->get_path($new_path);
		$new_path = rtrim($new_path, '\\/').DS.$info['basename'];

		$return = $this->area->rename($this->path, $new_path);
		$return and $this->path = $new_path;

		return $return;
	}

	/**
	 * Copy file
	 *
	 * @param	string	path to target directory, must be valid
	 * @return	bool
	 */
	public function copy($new_path)
	{
		$info = pathinfo($this->path);
		$new_path = $this->area->get_path($new_path);

		$new_path = rtrim($new_path, '\\/').DS.$info['basename'];

		return $this->area->copy($this->path, $new_path);
	}

	/**
	 * Update contents
	 *
	 * @param	mixed	new file contents
	 * @return	bool
	 */
	public function update($new_content)
	{
		$info = pathinfo($this->path);
		return $this->area->update($info['dirname'], $info['basename'], $new_content, $this);
	}

	/**
	 * Delete file
	 *
	 * @return	bool
	 */
	public function delete()
	{
		// should also destroy object but not possible in PHP right?
		return $this->area->delete($this->path);
	}

	/**
	 * Get the url.
	 *
	 * @return	bool
	 */
	public function get_url()
	{
		return $this->area->get_url($this->path);
	}

	/**
	 * Get the file's permissions.
	 *
	 * @return	string	file permissions
	 */
	public function get_permissions()
	{
		return $this->area->get_permissions($this->path);
	}

	/**
	 * Get the file's created or modified timestamp.
	 *
	 * @param	string	$type	modified or created
	 * @return	int		Unix Timestamp
	 */
	public function get_time($type = 'modified')
	{
		return $this->area->get_time($this->path, $type);
	}

	/**
	 * Get the file's size.
	 *
	 * @param	string	$type	modified or created
	 * @return	int		Unix Timestamp
	 */
	public function get_size()
	{
		return $this->area->get_size($this->path);
	}
}

