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

abstract class Image_Driver
{

	protected $image_fullpath  = null;
	protected $image_directory = null;
	protected $image_filename  = null;
	protected $image_extension = null;
	protected $new_extension   = null;
	protected $config          = array();
	protected $queued_actions  = array();
	protected $accepted_extensions;

	public function __construct($config)
	{
		\Config::load('image', true);
		if (is_array($config))
		{
			$this->config = array_merge(\Config::get('image'), $config);
		}
		else
		{
			$this->config = \Config::get('image');
		}
		$this->debug("Image Class was initialized using the " . $this->config['driver'] . " driver.");
	}
	/**
	 * Accepts configuration in either an array (as $index) or a pairing using $index and $value
	 *
	 * @param   string  $index  The index to be set, or an array of configuration options.
	 * @param   mixed   $value  The value to be set if $index is not an array.
	 * @return  Image_Driver
	 */
	public function config($index = null, $value = null)
	{
		if (is_array($index))
		{
			if (isset($index['driver']))
			{
				throw new \RuntimeException("The driver cannot be changed after initialization!");
			}
			$this->config = array_merge($this->config, $index);
		}
		elseif ($index != null)
		{
			if ($index == 'driver')
			{
				throw new \RuntimeException("The driver cannot be changed after initialization!");
			}
			$this->config[$index] = $value;
		}

		return $this;
	}

	/**
	 * Exectues the presets set in the config. Additional parameters replace the $1, $2, ect.
	 *
	 * @param   string  $name  The name of the preset.
	 * @return  Image_Driver
	 */
	public function preset($name)
	{
		$vars = func_get_args();
		if (isset($this->config['presets'][$name]))
		{
			$old_config   = $this->config;
			$this->config = array_merge($this->config, $this->config['presets'][$name]);
			foreach ($this->config['actions'] AS $action)
			{
				$func = $action[0];
				array_shift($action);
				for ($i = 0; $i < count($action); $i++)
				{
					for ($x = count($vars) - 1; $x >= 0; $x--)
					{
						$action[$i] = preg_replace('#\$' . $x . '#', $vars[$x], $action[$i]);
					}
				}
				call_user_func_array(array($this, $func), $action);
			}
			$this->config = $old_config;
		}
		else
		{
			throw new \InvalidArgumentException("Could not load preset $name, you sure it exists?");
		}
		return $this;
	}

	/**
	 * Loads the image and checks if its compatible.
	 *
	 * @param   string  $filename								The file to load
	 * @param   string  $return_data						Decides if it should return the images data, or just "$this".
	 * @param   mixed   $force_extension				Decides if it should force the extension witht this (or false)
	 * @return  Image_Driver
	 */
	public function load($filename, $return_data = false, $force_extension = false)
	{
		// First check if the filename exists
		$filename = realpath($filename);
		$return = array(
			'filename'    => $filename,
			'return_data' => $return_data
		);
		if (file_exists($filename))
		{
			// Check the extension
			$ext = $this->check_extension($filename, false, $force_extension);
			if ($ext !== false)
			{
				$return = array_merge($return, array(
					'image_fullpath'  => $filename,
					'image_directory' => dirname($filename),
					'image_filename'  => basename($filename),
					'image_extension' => $ext
				));
				if ( ! $return_data)
				{
					$this->image_fullpath = $filename;
					$this->image_directory = dirname($filename);
					$this->image_filename = basename($filename);
					$this->image_extension = $ext;
				}
			}
			else
			{
				throw new \RuntimeException("The library does not support this filetype for $filename.");
			}
		}
		else
		{
			throw new \OutOfBoundsException("Image file $filename does not exist.");
		}
		return $return;
	}

	/**
	 * Crops the image using coordinates or percentages.
	 *
	 * Positive whole numbers or percentages are coordinates from the top left.
	 *
	 * Negative whole numbers or percentages are coordinates from the bottom right.
	 *
	 * @param   integer  $x1  X-Coordinate for first set.
	 * @param   integer  $y1  Y-Coordinate for first set.
	 * @param   integer  $x2  X-Coordinate for second set.
	 * @param   integer  $y2  Y-Coordinate for second set.
	 * @return  Image_Driver
	 */
	public function crop($x1, $y1, $x2, $y2)
	{
		$this->queue('crop', $x1, $y1, $x2, $y2);
		return $this;
	}

	/**
	 * Executes the crop event when the queue is ran.
	 *
	 * Formats the crop method input for use with driver specific methods
	 *
	 * @param   integer  $x1  X-Coordinate for first set.
	 * @param   integer  $y1  Y-Coordinate for first set.
	 * @param   integer  $x2  X-Coordinate for second set.
	 * @param   integer  $y2  Y-Coordinate for second set.
	 * @return  array    An array of variables for the specific driver.
	 */
	protected function _crop($x1, $y1, $x2, $y2)
	{
		$y1 === null and $y1 = $x1;
		$x2 === null and $x2 = "-" . $x1;
		$y2 === null and $y2 = "-" . $y1;

		$x1 = $this->convert_number($x1, true);
		$y1 = $this->convert_number($y1, false);
		$x2 = $this->convert_number($x2, true);
		$y2 = $this->convert_number($y2, false);

		return array(
			'x1' => $x1,
			'y1' => $y1,
			'x2' => $x2,
			'y2' => $y2
		);
	}

	/**
	 * Resizes the image. If the width or height is null, it will resize retaining the original aspect ratio.
	 *
	 * @param   integer  $width   The new width of the image.
	 * @param   integer  $height  The new height of the image.
	 * @param   boolean  $keepar  If false, allows stretching of the image.
	 * @param   boolean  $pad     Adds padding to the image when resizing.
	 * @return  Image_Driver
	 */
	public function resize($width, $height = null, $keepar = true, $pad = false)
	{
		$this->queue('resize', $width, $height, $keepar, $pad);
		return $this;
	}


	/**
	 * Creates a vertical / horizontal or both mirror image.
	 *
	 * @access public
	 * @param mixed $direction 'vertical', 'horizontal', 'both'
	 * @return Image_Driver
	 */
	public function flip($direction)
	{
		$this->queue('flip', $direction);
		return $this;
	}

	/**
	 * Executes the resize event when the queue is ran.
	 *
	 * Formats the resize method input for use with driver specific methods.
	 *
	 * @param   integer  $width   The new width of the image.
	 * @param   integer  $height  The new height of the image.
	 * @param   boolean  $keepar  If false, allows stretching of the image.
	 * @param   boolean  $pad     Adds padding to the image when resizing.
	 * @return  array    An array of variables for the specific driver.
	 */
	protected function _resize($width, $height = null, $keepar = true, $pad = true)
	{
		if ($height == null or $width == null)
		{
			if ($height == null and substr($width, -1) == '%')
			{
				$height = $width;
			}
			elseif (substr($height, -1) == '%' and $width == null)
			{
				$width = $height;
			}
			else
			{
				$sizes = $this->sizes();
				if ($height == null and $width != null)
				{
					$height = $width * ($sizes->height / $sizes->width);
				}
				elseif ($height != null and $width == null)
				{
					$width = $height * ($sizes->width / $sizes->height);
				}
				else
				{
					throw new \InvalidArgumentException("Width and height cannot be null.");
				}
			}
		}

		$origwidth  = $this->convert_number($width, true);
		$origheight = $this->convert_number($height, false);
		$width      = $origwidth;
		$height     = $origheight;
		$sizes      = $this->sizes();
		$x = 0;
		$y = 0;
		if ($keepar)
		{
			// See which is the biggest ratio
			if (function_exists('bcdiv'))
			{
				$width_ratio  = bcdiv((float) $width, $sizes->width, 10);
				$height_ratio = bcdiv((float) $height, $sizes->height, 10);
				$compare = bccomp($width_ratio, $height_ratio, 10);
				if ($compare > -1)
				{
					$height = ceil((float) bcmul($sizes->height, $height_ratio, 10));
					$width = ceil((float) bcmul($sizes->width, $height_ratio, 10));
				}
				else
				{
					$height = ceil((float) bcmul($sizes->height, $width_ratio, 10));
					$width = ceil((float) bcmul($sizes->width, $width_ratio, 10));
				}
			}
			else
			{
				$width_ratio  = $width / $sizes->width;
				$height_ratio = $height / $sizes->height;
				if ($width_ratio >= $height_ratio)
				{
					$height = ceil($sizes->height * $height_ratio);
					$width = ceil($sizes->width * $height_ratio);
				}
				else
				{
					$height = ceil($sizes->height * $width_ratio);
					$width = ceil($sizes->width * $width_ratio);
				}
			}
		}

		if ($pad)
		{
			$x = floor(($origwidth - $width) / 2);
			$y = floor(($origheight - $height) / 2);
		}
		else
		{
			$origwidth  = $width;
			$origheight = $height;
		}

		return array(
			'width'   => $width,
			'height'  => $height,
			'cwidth'  => $origwidth,
			'cheight' => $origheight,
			'x' => $x,
			'y' => $y
		);
	}

	public function crop_resize($width, $height = null)
	{
		is_null($height) and $height = $width;
		$this->queue('crop_resize', $width, $height);
		return $this;
	}

	protected function _crop_resize($width, $height)
	{
		// Determine the crop size
		$sizes   = $this->sizes();
		$width   = $this->convert_number($width, true);
		$height  = $this->convert_number($height, false);

		if (function_exists('bcdiv'))
		{
			if (bccomp(bcdiv($sizes->width, $width, 10), bcdiv($sizes->height, $height, 10), 10) < 1)
			{
				$this->_resize($width, 0, true, false);
			}
			else
			{
				$this->_resize(0, $height, true, false);
			}
		}
		else
		{
			if ($sizes->width / $width < $sizes->height / $height)
			{
				$this->_resize($width, 0, true, false);
			}
			else
			{
				$this->_resize(0, $height, true, false);
			}
		}

		$sizes = $this->sizes();
		$y = floor(max(0, $sizes->height - $height) / 2);
		$x = floor(max(0, $sizes->width - $width) / 2);
		$this->_crop($x, $y, $x + $width, $y + $height);
	}

	/**
	 * Rotates the image
	 *
	 * @param   integer  $degrees  The degrees to rotate, negatives integers allowed.
	 * @return  Image_Driver
	 */
	public function rotate($degrees)
	{
		$this->queue('rotate', $degrees);
		return $this;
	}

	/**
	 * Executes the rotate event when the queue is ran.
	 *
	 * Formats the rotate method input for use with driver specific methods
	 *
	 * @param   integer  $degrees  The degrees to rotate, negatives integers allowed.
	 * @return  array    An array of variables for the specific driver.
	 */
	protected function _rotate($degrees)
	{
		$degrees %= 360;
		if ($degrees < 0)
		{
			$degrees = 360 + $degrees;
		}
		return array(
			'degrees' => $degrees
		);
	}

	/**
	 * Adds a watermark to the image.
	 *
	 * @param   string   $filename  The filename of the watermark file to use.
	 * @param   string   $position  The position of the watermark, ex: "bottom right", "center center", "top left"
	 * @param   integer  $padding   The amount of padding (in pixels) from the position.
	 * @return  Image_Driver
	 */
	public function watermark($filename, $position, $padding = 5)
	{
		$this->queue('watermark', $filename, $position, $padding);
		return $this;
	}

	/**
	 * Executes the watermark event when the queue is ran.
	 *
	 * Formats the watermark method input for use with driver specific methods
	 *
	 * @param   string   $filename  The filename of the watermark file to use.
	 * @param   string   $position  The position of the watermark, ex: "bottom right", "center center", "top left"
	 * @param   integer  $padding   The amount of padding (in pixels) from the position.
	 * @return  array    An array of variables for the specific driver.
	 */
	protected function _watermark($filename, $position, $padding = 5)
	{
		$filename = realpath($filename);
		$return = false;
		if (file_exists($filename) and $this->check_extension($filename, false))
		{
			$x = 0;
			$y = 0;
			$wsizes = $this->sizes($filename);
			$sizes  = $this->sizes();
			// Get the x and y  positions.
			list($ypos, $xpos) = explode(' ', $position);
			switch ($xpos)
			{
				case 'left':
					$x = $padding;
				break;
				case 'middle':
				case 'center':
					$x = ($sizes->width / 2) - ($wsizes->width / 2);
				break;
				case 'right':
					$x = $sizes->width - $wsizes->width - $padding;
				break;
			}
			switch ($ypos)
			{
				case 'top':
					$y = $padding;
				break;
				case 'middle':
				case 'center':
					$y = ($sizes->height / 2) - ($wsizes->height / 2);
				break;
				case 'bottom':
					$y = $sizes->height - $wsizes->height - $padding;
				break;
			}
			$this->debug("Watermark being placed at $x,$y");
			$return = array(
				'filename' => $filename,
				'x' => $x,
				'y' => $y,
				'padding' => $padding
			);
		}
		return $return;
	}

	/**
	 * Adds a border to the image.
	 *
	 * @param   integer  $size   The side of the border, in pixels.
	 * @param   string   $color  A hexadecimal color.
	 * @return  Image_Driver
	 */
	public function border($size, $color = null)
	{
		$this->queue('border', $size, $color);
		return $this;
	}

	/**
	 * Executes the border event when the queue is ran.
	 *
	 * Formats the border method input for use with driver specific methods
	 *
	 * @param   integer  $size   The side of the border, in pixels.
	 * @param   string   $color  A hexadecimal color.
	 * @return  array    An array of variables for the specific driver.
	 */
	protected function _border($size, $color = null)
	{
		empty($color) and $color = $this->config['bgcolor'];

		return array(
			'size' => $size,
			'color' => $color
		);
	}

	/**
	 * Masks the image using the alpha channel of the image input.
	 *
	 * @param   string  $maskimage  The location of the image to use as the mask
	 * @return  Image_Driver
	 */
	public function mask($maskimage)
	{
		$this->queue('mask', $maskimage);
		return $this;
	}

	/**
	 * Executes the mask event when the queue is ran.
	 *
	 * Formats the mask method input for use with driver specific methods
	 *
	 * @param   string  $maskimage  The location of the image to use as the mask
	 * @return  array   An array of variables for the specific driver.
	 */
	protected function _mask($maskimage)
	{
		return array(
			'maskimage' => $maskimage
		);
	}

	/**
	 * Adds rounded corners to the image.
	 *
	 * @param   integer  $radius
	 * @param   integer  $sides      Accepts any combination of "tl tr bl br" separated by spaces, or null for all sides
	 * @param   integer  $antialias  Sets the antialias range.
	 * @return  Image_Driver
	 */
	public function rounded($radius, $sides = null, $antialias = null)
	{
		$this->queue('rounded', $radius, $sides, $antialias);
		return $this;
	}

	/**
	 * Executes the rounded event when the queue is ran.
	 *
	 * Formats the rounded method input for use with driver specific methods
	 *
	 * @param   integer  $radius
	 * @param   integer  $sides      Accepts any combination of "tl tr bl br" separated by spaces, or null for all sides
	 * @param   integer  $antialias  Sets the antialias range.
	 * @return  array    An array of variables for the specific driver.
	 */
	protected function _rounded($radius, $sides, $antialias)
	{
		$radius < 0 and $radius = 0;
		$tl = $tr = $bl = $br = $sides == null;

		if ($sides != null)
		{
			$sides = explode(' ', $sides);
			foreach ($sides as $side)
			{
				if ($side == 'tl' or $side == 'tr' or $side == 'bl' or $side == 'br')
				{
					$$side = true;
				}
			}
		}
		$antialias == null and $antialias = 1;

		return array(
			'radius' => $radius,
			'tl' => $tl,
			'tr' => $tr,
			'bl' => $bl,
			'br' => $br,
			'antialias' => $antialias
		);
	}

	/**
	 * Turns the image into a grayscale version
	 *
	 * @return  Image_Driver
	 */
	public function grayscale()
	{
		$this->queue('grayscale');
		return $this;
	}

	/**
	 * Executes the grayscale event when the queue is ran.
	 */
	abstract protected function _grayscale();

	/**
	 * Saves the image, and optionally attempts to set permissions
	 *
	 * @param   string  $filename     The location where to save the image.
	 * @param   string  $permissions  Allows unix style permissions
	 * @return  array
	 */
	public function save($filename, $permissions = null)
	{
		$directory = dirname($filename);
		if ( ! is_dir($directory))
		{
			throw new \OutOfBoundsException("Could not find directory \"$directory\"");
		}

		if ( ! $this->check_extension($filename, true))
		{
			$filename .= "." . $this->image_extension;
		}
		// Touch the file
		if ( ! touch($filename))
		{
			throw new \RuntimeException("Do not have permission to write to \"$filename\"");
		}

		// Set the new permissions
		if ($permissions != null and ! chmod($filename, $permissions))
		{
			throw new \RuntimeException("Could not set permissions on the file.");
		}

		$this->debug("", "Saving image as <code>$filename</code>");
		return array(
			'filename' => $filename
		);
	}

	/**
	 * Saves the file in the original location, adding the append and prepend to the filename.
	 *
	 * @param   string   $append       The string to append to the filename
	 * @param   string   $prepend      The string to prepend to the filename
	 * @param   string   $extension    The extension to save the image as, null defaults to the loaded images extension.
	 * @param   integer  $permissions  The permissions to attempt to set on the file.
	 * @return  Image_Driver
	 */
	public function save_pa($append, $prepend = null, $extension = null, $permissions = null)
	{
		$filename = substr($this->image_filename, 0, -(strlen($this->image_extension) + 1));
		$fullpath = $this->image_directory.'/'.$append.$filename.$prepend.'.'.
			($extension !== null ? $extension : $this->image_extension);
		$this->save($fullpath, $permissions);
		return $this;
	}

	/**
	 * Outputs the file directly to the user.
	 *
	 * @param   string  $filetype  The extension type to use. Ex: png, jpg, gif
	 * @return  array
	 */
	public function output($filetype = null)
	{
		if ($filetype == null)
		{
			$filetype = $this->config['filetype'] == null ? $this->image_extension : $this->config['filetype'];
		}

		if ($this->check_extension($filetype, false))
		{
			if ( ! $this->config['debug'])
			{
				$mimetype = $filetype === 'jpg' ? 'jpeg' : $filetype;
				header('Content-Type: image/' . $mimetype);
			}
			$this->new_extension = $filetype;
		}
		else
		{
			throw new \FuelException("Image extension $filetype is unsupported.");
		}

		$this->debug('', "Outputting image as $filetype");
		return array(
			'filetype' => $filetype
		);
	}

	/**
	 * Returns sizes for the currently loaded image, or the image given in the $filename.
	 *
	 * @param   string  $filename  The location of the file to get sizes for.
	 * @return  object  An object containing width and height variables.
	 */
	abstract public function sizes($filename = null);

	/**
	 * Adds a background to the image using the 'bgcolor' config option.
	 */
	abstract protected function add_background();

	/**
	 * Creates a new color usable by all drivers.
	 *
	 * @param   string   $hex    The hex code of the color
	 * @return  array    rgba representation of the hex and alpha values.
	 */
	protected function create_hex_color($hex)
	{
		if ($hex == null)
		{
			$red = 0;
			$green = 0;
			$blue = 0;
		}
		else
		{
			// Check if theres a # in front
			if (substr($hex, 0, 1) == '#')
			{
				$hex = substr($hex, 1);
			}

			// Break apart the hex
			if (strlen($hex) == 6)
			{
				$red   = hexdec(substr($hex, 0, 2));
				$green = hexdec(substr($hex, 2, 2));
				$blue  = hexdec(substr($hex, 4, 2));
			}
			else
			{
				$red   = hexdec(substr($hex, 0, 1).substr($hex, 0, 1));
				$green = hexdec(substr($hex, 1, 1).substr($hex, 1, 1));
				$blue  = hexdec(substr($hex, 2, 1).substr($hex, 2, 1));
			}
		}

		return array(
			'red' => $red,
			'green' => $green,
			'blue' => $blue,
		);
	}

	/**
	 * Checks if the extension is accepted by this library, and if its valid sets the $this->image_extension variable.
	 *
	 * @param   string   $filename
	 * @param   boolean  $writevar					Decides if the extension should be written to $this->image_extension
	 * @param   mixed		 $force_extension		Decides if the extension should be overridden with this (or false)
	 * @return  boolean
	 */
	protected function check_extension($filename, $writevar = true, $force_extension = false)
	{
		$return = false;

		if ($force_extension !== false and in_array($force_extension, $this->accepted_extensions))
		{
			return $force_extension;
		}

		foreach ($this->accepted_extensions as $ext)
		{
			if (strtolower(substr($filename, strlen($ext) * -1)) == strtolower($ext))
			{
				$writevar and $this->image_extension = $ext;
				$return = $ext;
			}
		}
		return $return;
	}

	/**
	 * Converts percentages, negatives, and other values to absolute integers.
	 *
	 * @param   string   $input
	 * @param   boolean  $x  Determines if the number relates to the x-axis or y-axis.
	 * @return  integer  The converted number, usable with the image being edited.
	 */
	protected function convert_number($input, $x = null)
	{
		// Sanitize double negatives
		$input = str_replace('--', '', $input);

		$orig = $input;
		$sizes = $this->sizes();
		$size = $x ? $sizes->width : $sizes->height;
		// Convert percentages to absolutes
		if (substr($input, -1) == '%')
		{
			$input = floor((substr($input, 0, -1) / 100) * $size);
		}
		// Negatives are based off the bottom right
		if ($x !== null and $input < 0)
		{
			$input = $size + $input;
		}
		return $input;
	}

	/**
	 * Queues a function to run at a later time.
	 *
	 * @param  string  $function  The name of the function to be ran, without the leading _
	 */
	protected function queue($function)
	{
		$func = func_get_args();
		$tmpfunc = array();
		for ($i = 0; $i < count($func); $i++)
		{
			$tmpfunc[$i] = var_export($func[$i], true);
		}

		$this->debug("Queued <code>" . implode(", ", $tmpfunc) . "</code>");
		$this->queued_actions[] = $func;
	}

	/**
	 * Runs all queued actions on the loaded image.
	 *
	 * @param  boolean  $clear  Decides if the queue should be cleared once completed.
	 */
	public function run_queue($clear = null)
	{
		foreach ($this->queued_actions as $action)
		{
			$tmpfunc = array();
			for ($i = 0; $i < count($action); $i++)
			{
				$tmpfunc[$i] = var_export($action[$i], true);
			}
			$this->debug('', "<b>Executing <code>" . implode(", ", $tmpfunc) . "</code></b>");
			call_user_func_array(array(&$this, '_' . $action[0]), array_slice($action, 1));
		}
		if (($clear === null and $this->config['clear_queue']) or $clear === true)
		{
			$this->queued_actions = array();
		}
	}

	/**
	 * Reloads the image.
	 *
	 * @return  Image_Driver
	 */
	public function reload()
	{
		$this->debug("Reloading was called!");
		$this->load($this->image_fullpath);
		return $this;
	}

	/**
	 * Used for debugging image output.
	 *
	 * @param  string  $message
	 */
	protected function debug()
	{
		if ($this->config['debug'])
		{
			$messages = func_get_args();
			foreach ($messages as $message)
			{
				echo '<div>' . $message . '&nbsp;</div>';
			}
		}
	}
}

