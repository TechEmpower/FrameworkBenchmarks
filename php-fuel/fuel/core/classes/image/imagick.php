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

class Image_Imagick extends \Image_Driver
{

	protected $accepted_extensions = array('png', 'gif', 'jpg', 'jpeg');
	protected $imagick = null;

	public function load($filename, $return_data = false, $force_extension = false)
	{
		extract(parent::load($filename, $return_data, $force_extension));

		if ($this->imagick == null)
		{
			$this->imagick = new \Imagick();
		}

		$this->imagick->readImage($filename);

		return $this;
	}

	protected function _crop($x1, $y1, $x2, $y2)
	{
		extract(parent::_crop($x1, $y1, $x2, $y2));

		$width = $x2 - $x1;
		$height = $y2 - $y1;

		$this->debug("Cropping image ".$width."x".$height."+$x1+$y1 based on coords ($x1, $y1), ($x2, $y2)");

		$this->imagick->cropImage($width, $height, $x1, $y1);
		$this->imagick->setImagePage(0, 0, 0, 0);
	}

	protected function _resize($width, $height = null, $keepar = true, $pad = true)
	{
		extract(parent::_resize($width, $height, $keepar, $pad));

		$this->imagick->scaleImage($width, $height, $keepar);

		if ($pad)
		{
			$tmpimage = new \Imagick();
			$tmpimage->newImage($cwidth, $cheight, $this->create_color('#000', 0), 'png');
			$tmpimage->compositeImage($this->imagick, \Imagick::COMPOSITE_DEFAULT, ($cwidth-$width) / 2, ($cheight-$height) / 2);
			$this->imagick = $tmpimage;
		}
	}

	protected function _rotate($degrees)
	{
		extract(parent::_rotate($degrees));

		$this->imagick->rotateImage($this->create_color('#000', 0), $degrees);
	}

	protected function _watermark($filename, $position, $padding = 5)
	{
		extract(parent::_watermark($filename, $position, $padding));
		$wmimage = new \Imagick();
		$wmimage->readImage($filename);
		$wmimage->evaluateImage(\Imagick::EVALUATE_MULTIPLY, $this->config['watermark_alpha'] / 100, \Imagick::CHANNEL_ALPHA);
		$this->imagick->compositeImage($wmimage, \Imagick::COMPOSITE_DEFAULT, $x, $y);
	}

	protected function _flip($direction)
	{
		switch ($direction)
		{
			case 'vertical':
			$this->imagick->flipImage();
			break;

			case 'horizontal':
			$this->imagick->flopImage();
			break;

			case 'both':
			$this->imagick->flipImage();
			$this->imagick->flopImage();
			break;

			default: return false;
		}
	}

	protected function _border($size, $color = null)
	{
		extract(parent::_border($size, $color));

		$this->imagick->borderImage($this->create_color($color, 100), $size, $size);
	}

	protected function _mask($maskimage)
	{
		extract(parent::_mask($maskimage));
		$wmimage = new \Imagick();
		$wmimage->readImage($maskimage);
		$wmimage->setImageMatte(false);
		$this->imagick->compositeImage($wmimage, \Imagick::COMPOSITE_COPYOPACITY, 0, 0);
	}

	protected function _rounded($radius, $sides, $antialias = 0)
	{
		extract(parent::_rounded($radius, $sides, null));

		$sizes = $this->sizes();
		$sizes->width_half = $sizes->width / 2;
		$sizes->height_half = $sizes->height / 2;

		if ( ! $tl)
		{
			$tlimage = $this->imagick->clone();
			$tlimage->cropImage($sizes->width_half, $sizes->height_half, 0, 0);
		}

		if ( ! $tr)
		{
			$trimage = $this->imagick->clone();
			$trimage->cropImage($sizes->width_half, $sizes->height_half, $sizes->width_half, 0);
		}

		if ( ! $bl)
		{
			$blimage = $this->imagick->clone();
			$blimage->cropImage($sizes->width_half, $sizes->height_half, 0, $sizes->height_half);
		}

		if ( ! $br)
		{
			$brimage = $this->imagick->clone();
			$brimage->cropImage($sizes->width_half, $sizes->height_half, $sizes->width_half, $sizes->height_half);
		}

		$this->imagick->roundCorners($radius, $radius);

		if ( ! $tl)
		{
			$this->imagick->compositeImage($tlimage, \Imagick::COMPOSITE_DEFAULT, 0, 0);
		}

		if ( ! $tr)
		{
			$this->imagick->compositeImage($trimage, \Imagick::COMPOSITE_DEFAULT, $sizes->width_half, 0);
		}

		if ( ! $bl)
		{
			$this->imagick->compositeImage($blimage, \Imagick::COMPOSITE_DEFAULT, 0, $sizes->height_half);
		}

		if ( ! $br)
		{
			$this->imagick->compositeImage($brimage, \Imagick::COMPOSITE_DEFAULT, $sizes->width_half, $sizes->height_half);
		}
	}

	protected function _grayscale()
	{
		$this->imagick->setImageType(\Imagick::IMGTYPE_GRAYSCALEMATTE);
	}

	public function sizes($filename = null, $usecache = true)
	{
		if ($filename === null)
		{
			return (object) array(
				'width'  => $this->imagick->getImageWidth(),
				'height' => $this->imagick->getImageHeight()
			);
		}

		$tmpimage = new \Imagick();
		$tmpimage->readImage($filename);
		return (object) array(
			'width'  => $tmpimage->getImageWidth(),
			'height' => $tmpimage->getImageHeight()
		);
	}

	public function save($filename, $permissions = null)
	{
		extract(parent::save($filename, $permissions));

		$this->run_queue();
		$this->add_background();

		$filetype = $this->image_extension;

		if ($filetype == 'jpg' or $filetype == 'jpeg')
		{
			$filetype = 'jpeg';
		}

		if ($this->imagick->getImageFormat() != $filetype)
		{
			$this->imagick->setImageFormat($filetype);
		}

		if($this->imagick->getImageFormat() == 'jpeg' and $this->config['quality'] != 100)
		{
			$this->imagick->setImageCompression(\Imagick::COMPRESSION_JPEG);
			$this->imagick->setImageCompressionQuality($this->config['quality']);
			$this->imagick->stripImage();
		}

		file_put_contents($filename, $this->imagick->getImageBlob());

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

		if ($filetype == 'jpg' or $filetype == 'jpeg')
		{
			$filetype = 'jpeg';
		}

		if ($this->imagick->getImageFormat() != $filetype)
		{
			$this->imagick->setImageFormat($filetype);
		}

		if($this->imagick->getImageFormat() == 'jpeg' and $this->config['quality'] != 100)
		{
			$this->imagick->setImageCompression(\Imagick::COMPRESSION_JPEG);
			$this->imagick->setImageCompressionQuality($this->config['quality']);
			$this->imagick->stripImage();
		}

		if ( ! $this->config['debug'])
		{
			echo $this->imagick->getImageBlob();
		}

		return $this;
	}

	protected function add_background()
	{
		if($this->config['bgcolor'] != null)
		{
			$tmpimage = new \Imagick();
			$sizes = $this->sizes();
			$tmpimage->newImage($sizes->width, $sizes->height, $this->create_color($this->config['bgcolor'], $this->config['bgcolor'] == null ? 0 : 100), 'png');
			$tmpimage->compositeImage($this->imagick, \Imagick::COMPOSITE_DEFAULT, 0, 0);
			$this->imagick = $tmpimage;
		}
	}

	/**
	 * Creates a new color usable by Imagick.
	 *
	 * @param  string   $hex    The hex code of the color
	 * @param  integer  $alpha  The alpha of the color, 0 (trans) to 100 (opaque)
	 * @return string   rgba representation of the hex and alpha values.
	 */
	protected function create_color($hex, $alpha)
	{
		extract($this->create_hex_color($hex));
		return new \ImagickPixel('rgba('.$red.', '.$green.', '.$blue.', '.round($alpha / 100, 2).')');
	}
}
