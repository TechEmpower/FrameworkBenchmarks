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

class Image_Imagemagick extends \Image_Driver
{

	protected $image_temp = null;
	protected $accepted_extensions = array('png', 'gif', 'jpg', 'jpeg');
	protected $size_cache = null;
	protected $im_path = null;

	public function load($filename, $return_data = false, $force_extension = false)
	{
		extract(parent::load($filename, $return_data, $force_extension));

		$this->clear_sizes();
		if (empty($this->image_temp))
		{
			do
			{
				$this->image_temp = $this->config['temp_dir'].substr($this->config['temp_append'].md5(time() * microtime()), 0, 32).'.png';
			}
			while (file_exists($this->image_temp));
		}
		elseif (file_exists($this->image_temp))
		{
			$this->debug('Removing previous temporary image.');
			unlink($this->image_temp);
		}
		$this->debug('Temp file: '.$this->image_temp);
		if (!file_exists($this->config['temp_dir']) || !is_dir($this->config['temp_dir']))
		{
			throw new \RuntimeException("The temp directory that was given does not exist.");
		}
		elseif (!touch($this->config['temp_dir'] . $this->config['temp_append'] . '_touch'))
		{
			throw new \RuntimeException("Could not write in the temp directory.");
		}
		$this->exec('convert', '"'.$image_fullpath.'"[0] "'.$this->image_temp.'"');

		return $this;
	}

	protected function _crop($x1, $y1, $x2, $y2)
	{
		extract(parent::_crop($x1, $y1, $x2, $y2));
		$image = '"'.$this->image_temp.'"';
		$this->exec('convert', $image.' -crop '.($x2 - $x1).'x'.($y2 - $y1).'+'.$x1.'+'.$y1.' +repage '.$image);
		$this->clear_sizes();
	}

	protected function _resize($width, $height = null, $keepar = true, $pad = true)
	{
		extract(parent::_resize($width, $height, $keepar, $pad));

		$image = '"'.$this->image_temp.'"';
		$this->exec('convert', "-define png:size=".$cwidth."x".$cheight." ".$image." ".
			"-background none ".
			"-resize \"".($pad ? $width : $cwidth)."x".($pad ? $height : $cheight)."!\" ".
			"-gravity center ".
			"-extent ".$cwidth."x".$cheight." ".$image);
		$this->clear_sizes();
	}

	protected function _rotate($degrees)
	{
		extract(parent::_rotate($degrees));

		$image = '"'.$this->image_temp.'"';
		$this->exec('convert', $image." -background none -virtual-pixel background +distort ScaleRotateTranslate ".$degrees." +repage ".$image);

		$this->clear_sizes();
	}

	protected function _flip($direction)
	{
		switch ($direction)
		{
			case 'vertical':
			$arg = '-flip';
			break;

			case 'horizontal':
			$arg = '-flop';
			break;

			case 'both':
			$arg = '-flip -flop';
			break;

			default: return false;
		}
		$image = '"'.$this->image_temp.'"';
		$this->exec('convert', $image.' '.$arg.' '.$image);
	}

	protected function _watermark($filename, $position, $padding = 5)
	{
		$values = parent::_watermark($filename, $position, $padding);
		if ($values == false)
		{
			throw new \InvalidArgumentException("Watermark image not found or invalid filetype.");
		}

		extract($values);
		$x >= 0 and $x = '+'.$x;
		$y >= 0 and $y = '+'.$y;

		$this->exec(
			'composite',
			'-compose atop -geometry '.$x.$y.' '.
			'-dissolve '.$this->config['watermark_alpha'].'% '.
			'"'.$filename.'" "'.$this->image_temp.'" '.$image
		);
	}

	protected function _border($size, $color = null)
	{
		extract(parent::_border($size, $color));

		$image = '"'.$this->image_temp.'"';
		$color = $this->create_color($color, 100);
		$command = $image.' -compose copy -bordercolor '.$color.' -border '.$size.'x'.$size.' '.$image;
		$this->exec('convert', $command);

		$this->clear_sizes();
	}

	protected function _mask($maskimage)
	{
		extract(parent::_mask($maskimage));

		$mimage = '"'.$maskimage.'"';
		$image = '"'.$this->image_temp.'"';
		$command = $image.' '.$mimage.' +matte  -compose copy-opacity -composite '.$image;
		$this->exec('convert', $command);
	}

	/**
	 * Credit to Leif Åstrand <leif@sitelogic.fi> for the base of the round corners.
	 *
	 * Note there is a defect with this, as non-transparent corners get opaque circles of color. Maybe mask it with auto-generated corners?
	 *
	 * @link  http://www.imagemagick.org/Usage/thumbnails/#rounded
	 */
	protected function _rounded($radius, $sides, $antialias = 0)
	{
		extract(parent::_rounded($radius, $sides, null));

		$image = '"'.$this->image_temp.'"';
		$r = $radius;
		$command = $image." ( +clone -alpha extract ".
			( ! $tr ? '' : "-draw \"fill black polygon 0,0 0,$r $r,0 fill white circle $r,$r $r,0\" ")."-flip ".
			( ! $br ? '' : "-draw \"fill black polygon 0,0 0,$r $r,0 fill white circle $r,$r $r,0\" ")."-flop ".
			( ! $bl ? '' : "-draw \"fill black polygon 0,0 0,$r $r,0 fill white circle $r,$r $r,0\" ")."-flip ".
			( ! $tl ? '' : "-draw \"fill black polygon 0,0 0,$r $r,0 fill white circle $r,$r $r,0\" ").
			') -alpha off -compose CopyOpacity -composite '.$image;
		$this->exec('convert', $command);
	}

	protected function _grayscale()
	{
		$image = '"'.$this->image_temp.'"';
		$this->exec('convert', $image." -colorspace Gray ".$image);
	}

	public function sizes($filename = null, $usecache = true)
	{
		$is_loaded_file = $filename == null;
		if ( ! $is_loaded_file or $this->sizes_cache == null or !$usecache)
		{
			$reason = ($filename != null ? "filename" : ($this->size_cache == null ? 'cache' : 'option'));
			$this->debug("Generating size of image... (triggered by $reason)");

			if ($is_loaded_file and ! empty($this->image_temp))
			{
				$filename = $this->image_temp;
			}

			$output = $this->exec('identify', '-format "%[fx:w] %[fx:h]" "'.$filename.'"[0]');
			list($width, $height) = explode(" ", $output[0]);
			$return = (object) array(
				'width' => $width,
				'height' => $height
			);

			if ($is_loaded_file)
			{
				$this->sizes_cache = $return;
			}
			$this->debug("Sizes ".( !$is_loaded_file ? "for <code>$filename</code> " : "")."are now $width and $height");
		}
		else
		{
			$return = $this->sizes_cache;
		}

		return $return;
	}

	public function save($filename, $permissions = null)
	{
		extract(parent::save($filename, $permissions));

		$this->run_queue();
		$this->add_background();

		$filetype = $this->image_extension;
		$old = '"'.$this->image_temp.'"';
		$new = '"'.$filename.'"';

		if(($filetype == 'jpeg' or $filetype == 'jpg') and $this->config['quality'] != 100)
		{
			$quality = '"'.$this->config['quality'].'%"';
			$this->exec('convert', $old.' -quality '.$quality.' '.$new);
		}
		else
		{
			$this->exec('convert', $old.' '.$new);
		}

		if ($this->config['persistence'] === false)
		{
			$this->reload();
		}

		return $this;
	}

	public function output($filetype = null)
	{
		extract(parent::output($filetype));

		$this->run_queue();
		$this->add_background();

		$image = '"'.$this->image_temp.'"';

		if(($filetype == 'jpeg' or $filetype == 'jpg') and $this->config['quality'] != 100)
		{
			$quality = '"'.$this->config['quality'].'%"';
			$this->exec('convert', $image.' -quality '.$quality.' '.strtolower($filetype).':-', true);
		}
		elseif (substr($this->image_temp, -1 * strlen($filetype)) != $filetype)
		{
			if ( ! $this->config['debug'])
			{
				$this->exec('convert', $image.' '.strtolower($filetype).':-', true);
			}
		}
		else
		{
			if ( ! $this->config['debug'])
			{
				echo file_get_contents($this->image_temp);
			}
		}

		if ($this->config['persistence'] === false)
		{
			$this->reload();
		}

		return $this;
	}

	/**
	 * Cleared the currently loaded sizes, used to removed cached sizes.
	 */
	protected function clear_sizes()
	{
		$this->sizes_cache = null;
	}

	protected function add_background()
	{
		if ($this->config['bgcolor'] != null)
		{
			$bgcolor = $this->config['bgcolor'] == null ? '#000' : $this->config['bgcolor'];
			$image   = '"'.$this->image_temp.'"';
			$color   = $this->create_color($bgcolor, 100);
			$sizes   = $this->sizes();
			$command = '-size '.$sizes->width.'x'.$sizes->height.' '.'canvas:'.$color.' '.
				$image.' -composite '.$image;
			$this->exec('convert', $command);
		}
	}

	/**
	 * Executes the specified imagemagick executable and returns the output.
	 *
	 * @param   string   $program   The name of the executable.
	 * @param   string   $params    The parameters of the executable.
	 * @param   boolean  $passthru  Returns the output if false or pass it to browser.
	 * @return  mixed    Either returns the output or returns nothing.
	 */
	private function exec($program, $params, $passthru = false)
	{
		//  Determine the path
		$this->im_path = realpath($this->config['imagemagick_dir'].$program);
		if ( ! $this->im_path)
		{
			$this->im_path = realpath($this->config['imagemagick_dir'].$program.'.exe');
		}
		if ( ! $this->im_path)
		{
			throw new \RuntimeException("imagemagick executables not found in ".$this->config['imagemagick_dir']);
		}

		$command = $this->im_path." ".$params;
		$this->debug("Running command: <code>$command</code>");
		$code = 0;
		$output = null;

		$passthru ? passthru($command) : exec($command, $output, $code);

		if ($code != 0)
		{
			throw new \FuelException("imagemagick failed to edit the image. Returned with $code.<br /><br />Command:\n <code>$command</code>");
		}

		return $output;
	}

	/**
	 * Creates a new color usable by ImageMagick.
	 *
	 * @param   string   $hex    The hex code of the color
	 * @param   integer  $alpha  The alpha of the color, 0 (trans) to 100 (opaque)
	 * @return  string   rgba representation of the hex and alpha values.
	 */
	protected function create_color($hex, $alpha)
	{
		extract($this->create_hex_color($hex));
		return "\"rgba(".$red.", ".$green.", ".$blue.", ".round($alpha / 100, 2).")\"";
	}

	public function __destruct()
	{
		if (file_exists($this->image_temp))
		{
			unlink($this->image_temp);
		}
	}
}

