<?php namespace Core;
/**
 * CCF Image manipulation with less pain
 * I know the class is still pretty much a mess, i going to change this soon.
 ** 
 *
 * @package		ClanCatsFramework
 * @author		Mario Döring <mario@clancats.com>
 * @version		2.0
 * @copyright 	2010 - 2014 ClanCats GmbH
 *
 */
class CCImage 
{
	/**
	 * Currently supportet image types
	 *
	 * @var array
	 */
	private static $available_image_types = array( 'gif', 'png', 'jpg', 'jpeg' );
	
	/**
	 * Creates an new empty image
	 *
	 * @param int			$width
	 * @param int			$height
	 * @param string 		$type 		jpg|png|gif
	 *
	 * @return CCImage
	 */
	public static function blank( $width, $height, $type = null ) 
	{
		return new static( imagecreatetruecolor( $width, $height ), $type );
	}

	/**
	 * Create a new CCImage from upload
	 *
	 * @param string 		$key
	 * @param string 		$type		jpg|png|gif
	 *
	 * @return CCImage|false
	 */
	public static function upload( $key, $type = null )
	{
		return static::create( CCFile::upload_path( $key ), $type );
	}

	/**
	 * Create a image from file
	 *
	 * @param string 	$file 
	 * @param string 	$type		jpg|png|gif
	 *
	 * @return CCImage|false
	 */
	public static function create( $file, $type = null ) 
	{
		// when no type is given use the file extension
		if ( is_null( $type ) ) 
		{
			$type = CCStr::extension( $file );
			
			// validate type
			if ( !in_array( $type, static::$available_image_types ) )
			{
				$type = null;
			}
		}

		$image_data = getimagesize( $file );

		if ( $image_data === false ) 
		{
			return false;
		}

		$image = null;

		switch( $image_data['mime'] ) 
		{
			case 'image/gif':
				$image = imagecreatefromgif( $file );
			break;
			
			case 'image/jpeg';
				$image = imagecreatefromjpeg( $file );
			break;
			
			case 'image/png':
				$image = imagecreatefrompng( $file );
			break;
			
			default:
				// we dont support other image types
				return false;
			break;
		}
		
		// when the image type is still null we are going to use 
		// the mime type of the image 
		if ( is_null( $type ) )
		{
			$type = CCStr::suffix( $image_data['mime'], '/' );
		}

		return new static( $image, $type );
	}

	/**
	 * Create an image from string
	 *
	 * @param string 		$string 			The image data string
	 * @param string 		$type 			jpg|png|gif
	 *
	 * @return CCImage|false
	 */
	public static function string( $string, $type = null ) 
	{
		$image = imagecreatefromstring( $string );

		if ( $image !== false ) 
		{
			return new static( $image, $type );
		}

		return false;
	}

	/**
	 * Calculate the aspect ratio
	 *
	 * @param int	$width
	 * @param int 	$height
	 * @param bool	$proper
	 *
	 * @return string
	 *
	 * @thanks to: http://jonisalonen.com/2012/converting-decimal-numbers-to-ratios/
	 */
	public static function aspect_ratio( $width, $height, $proper = false ) 
	{
		$ratio = $width / $height;

		if ( !$proper ) 
		{
			return $ratio;
		}

		$tolerance = 1.e-6;
		$h1=1; $h2=0;
		$k1=0; $k2=1;
		$b = 1/$ratio;

		do {
			$b = 1/$b;
			$a = floor($b);
			$aux = $h1; $h1 = $a*$h1+$h2; $h2 = $aux;
			$aux = $k1; $k1 = $a*$k1+$k2; $k2 = $aux;
			$b = $b-$a;

		} while ( abs( $ratio-$h1 / $k1 ) > $ratio * $tolerance );

		return $h1.":".$k1;
	}

	/**
	 * image type default is png
	 *
	 * @var string
	 */
	public $type = null;

	/**
	 * The image width
	 *
	 * @var int
	 */
	public $width = 0;
	
	/**
	 * The image height
	 *
	 * @var int
	 */
	public $height = 0;
	
	/**
	 * The image context
	 *
	 * @var resource
	 */
	protected $image_context = null;

	/**
	 * CCImage constructor
	 *
	 * @param resource			$image_context
	 * @param string 			$type				jpg|png|gif
	 * @return void	
	 */
	public function __construct( $image_context, $type = null ) 
	{
		if ( is_resource( $image_context ) !== true ) 
		{
			throw new CCException( "CCImage - Invalid image context given." );
		}
		
		$this->set_type( $type );

		$this->image_context = $image_context;
		
		$this->reload_context_info();
	}
	
	/**
	 * Reload the image dimension etc.
	 *
	 * @return void
	 */
	protected function reload_context_info()
	{
		$this->width  = imagesx( $this->image_context );  
		$this->height = imagesy( $this->image_context );
	}
	
	/**
	 * Set the current image type
	 *
	 * @param string 		$type		The new image type
	 * @param string 		$overwrite	Should the image keep this type?
	 * @return string
	 */
	protected function set_type( $type, $overwrite = true )
	{
		if ( !is_null( $type ) )
		{
			if ( !in_array( $type, static::$available_image_types ) )
			{
				throw new CCException( "CCImage - Invalid image type '".$type."'." );
			}
			
			// don't allow jpg, set to jpeg
			if ( $type === 'jpg' )
			{
				$type = 'jpeg';
			}
			
			if ( $overwrite )
			{
				$this->type = $type;
			}
		}
		
		return $type;
	}

	/**
	 * Save the image 
	 *
	 * When you set the file to null the image will be send to the output buffer.
	 *
	 * Examples:
	 *     $image = CCImage::create( 'path/to/my/image.jpg' );
	 *     // save to file
	 *     $image->save( 'my/new/image.jpg' );
	 *     // quality (80%) and type (gif) 
	 *     $image->save( 'my/new/image.gif', 'gif', 80 );
	 *     // send to output buffer
	 *     $image->save( null );
	 *
	 * @param string		$file
	 * @param int		$quality 	between 1-100
	 * @param string		$type
	 *
	 * @return bool
	 */
	public function save( $file, $quality = null, $type = null ) 
	{
		$type = $this->set_type( $type );
		
		// create directory if not exists
		if ( !is_null( $file ) )
		{
			CCFile::mkdir( $file );
		}
		
		switch( $type ) 
		{
			// PNG images
			case 'png':
				if ( is_null( $quality ) ) 
				{
					$quality = -1;
				} else {
					$quality = ( $quality / 100 ) * 9;
				}
				return imagepng( $this->image_context, $file, $quality );
			break;
			
			// GIF images
			case 'gif':
				return imagegif( $this->image_context, $file );
			break;
			
			// JPEG images
			case 'jpeg':
			default:
				if ( is_null( $quality ) ) 
				{
					$quality = 90;
				}
				return imagejpeg( $this->image_context, $file, $quality );
			break;
		}
	}

	/**
	 * Send the image to the output buffer
	 *
	 * @param int		$quality
	 * @param string		$type		jpg|png|gif
	 * @return void
	 */
	public function stream( $quality = null, $type = null ) 
	{
		$this->save( null, $quality, $type );
	}

	/**
	 * Return the image data as string
	 *
	 * @param int		$quality
	 * @param string		$type		jpg|png|gif
	 * @return string
	 */
	public function stringify( $quality = null, $type = null ) 
	{
		ob_start(); $this->stream( $quality, $type ); return ob_get_clean();
	}

	/**
	 * Create a CCRespone of the image
	 *
	 * @param string 		$quality			The image quality
	 * @param string 		$type			jpg|png|gif
	 * @return CCresponse
	 */
	public function response( $quality = null, $type = null ) 
	{
		$response = CCResponse::create( $this->stringify( $quality, $type ) );
		
		if ( !is_null( $this->type ) )
		{
			$response->header( 'Content-Type', 'image/'.$this->type );
		}
		
		return $response;
	}

	/**
	 * Resize the image
	 * 
	 * Examples:
	 *     // simple resize
	 *     $image->resize( '200x150', 'fill' );
	 *     // does the same as
	 *     $image->resize( 200, 150, 'fill' );
	 *     // you can use auto values
	 *     $image->resize( 500, 'auto' );
	 * 
	 * @param int 		$width
	 * @param int		$height
	 * @param string		$mode
	 *
	 * @return self
	 */
	public function resize( $width, $height, $mode = null ) 
	{
		// check for alternative syntax 
		if ( strpos( $width, 'x' ) !== false ) 
		{
			// mode is the secound param
			$mode = $height;

			$dimensions = explode( 'x', $width );
			$width = $dimensions[0];
			$height = $dimensions[1];
		} 

		// default mode
		if ( is_null( $mode ) ) 
		{
			$mode = 'strict';
		}

		// auto width
		if ( $width == 'auto' ) 
		{
			$mode = 'portrait';
			// in this case the $height is the first param
			$width = $height;
		} 
		// auto height
		elseif ( $height == 'auto' ) 
		{
			$mode = 'landscape';
		} 

		$method = 'resize_'.$mode;

		if ( !method_exists( $this, $method ) ) 
		{
			throw new CCException( "CCImage::resize - Invalid resize method ".$mode."." );
		}

		return call_user_func_array( array( $this, $method ), array( $width, $height ) );
	}

	/**
	 * Resize the current image from width and keep aspect ratio
	 * 
	 * @param int 		$width
	 * @param int 		$height
	 * @return self
	 */
	public function resize_landscape( $width, $ignore_me ) 
	{
		// calculate height
		$height = $width * ( $this->height / $this->width );

		return $this->resize_strict( $width, $height );
	}

	/**
	 * Resize the current image from height and keep aspect ratio
	 * 
	 * @param int 		$width
	 * @param int 		$height
	 * @return self
	 */
	public function resize_portrait( $height, $ignore_me )
	{
		// calculate width
		$width = $height * ( $this->width / $this->height );

		return $this->resize_strict( $width, $height );
	}

	/**
	 * Resize the image that it fits into a size doesn't crop and does not add a border
	 * 
	 * @param int 		$width
	 * @param int 		$height
	 * @return self
	 */
	public function resize_max( $width, $height ) 
	{
		$new_width = $this->width;
		$new_height = $this->height;

		if ( $new_width > $width ) 
		{
			// set new with
			$new_width = $width;
			// calculate height
			$new_height = $new_width * ( $this->height / $this->width );
		}

		if ( $new_height > $height ) 
		{
			// set new height
			$new_height = $height;
			// calculate width
			$new_width = $new_height * ( $this->width / $this->height );
		}

		return $this->resize_strict( $new_width, $new_height );
	}

	/**
	 * Resize the image that it fits into a size doesn't crop adds a background layer
	 * 
	 * @param int 		$width
	 * @param int 		$height
	 * @return self
	 */
	public function resize_fit( $width, $height, $background_color = '#fff' ) 
	{
		$background = static::blank( $width, $height );

		// make out actual image max size
		static::resize_max( $width, $height );

		// make background white
		$background->fill_color( $background_color );

		// add the layer
		$background->add_layer( $this, 'center', 'middle' );

		// overwrite the image context 
		$this->image_context = $background->image_context;

		// update properties
		$this->width = $width; 
		$this->height = $height;

		// return self
		return $this;
	}

	/**
	 * Resize the image to fill the new size. This will crop your image.
	 * 
	 * @param int 		$width
	 * @param int 		$height
	 * @return self
	 *
	 * @thanks to: http://stackoverflow.com/questions/1855996/crop-image-in-php
	 */
	public function resize_fill( $width, $height ) 
	{
		$original_aspect = $this->width / $this->height;
		$thumb_aspect = $width / $height;
		
		if ( $original_aspect >= $thumb_aspect )
		{
		   $new_height = $height;
		   $new_width = $this->width / ($this->height / $height);
		}
		else
		{
		   $new_width = $width;
		   $new_height = $this->height / ($this->width / $width);
		}
		
		$x = 0 - ( $new_width - $width ) / 2;
		$y = 0 - ( $new_height - $height ) / 2;
		
		$result = imagecreatetruecolor( $width, $height );
		imagecopyresampled( $result, $this->image_context, $x, $y, 0, 0, $new_width, $new_height, $this->width, $this->height );
		
		// overwrite the image context 
		$this->image_context = $result;
		
		// update properties
		$this->reload_context_info();
		
		// return self
		return $this;
	}

	/**
	 * Resize the current image to strict dimensions
	 * 
	 * @param int 		$width
	 * @param int		$height
	 * @param string		$mode
	 *
	 * @return self
	 */
	public function resize_strict( $width, $height ) 
	{
		// check dimensions
		if ( !( $width > 0 ) || !( $height > 0 ) ) 
		{
			throw new CCException( "CCImage::resize_strict - width and height can't be smaller then 1" );
		}

		$result = imagecreatetruecolor( $width, $height );  
		imagecopyresampled( $result, $this->image_context, 0, 0, 0, 0, $width, $height, $this->width, $this->height ); 

		// overwrite the image context 
		$this->image_context = $result;

		// update properties
		$this->reload_context_info();

		// return self
		return $this;
	}
	
	/**
	 * Crop the current image
	 *
	 * This is a simplefied crop.
	 *
	 * @param int 		$x
	 * @param int 		$y 
	 * @param int		$width
	 * @param int 		$height
	 *
	 * @return self
	 */
	public function crop( $x, $y, $width, $height )
	{
		// check for auto
		if ( $x == 'center' || $x == 'auto' )
		{
			$x = ( $this->width / 2 ) - ( $width / 2 );
		}
		
		if ( $y == 'middle' || $y == 'auto' )
		{
			$y = ( $this->height / 2 ) - ( $height / 2 );
		}
		
		$result = imagecreatetruecolor( $width, $height );  
		ImageCopyResampled( $result, $this->image_context, 0, 0, $x, $y, $width, $height, $width, $height );
		
		$this->image_context = $result;
		
		// update properties
		$this->reload_context_info();
		
		// return self
		return $this;
	}

	/**
	 * add an layer to the current image
	 *
	 * @param CCImage 		$image
	 * @param int			$x
	 * @param int			$y
	 *
	 * @return self
	 */
	public function add_layer( CCImage $image, $x = 0, $y = 0 ) 
	{
		// auto values
		if ( $x == 'center' || $x == 'auto' )
		{
			$x = ( $this->width / 2 ) - ( $image->width / 2 );
		}
		elseif ( $x == 'left' )
		{
			$x = 0;
		}
		elseif ( $x == 'right' )
		{
			$x = $this->width - $image->width;
		}
		
		if ( $y == 'middle' || $y == 'auto' )
		{
			$y = ( $this->height / 2 ) - ( $image->height / 2 );
		}
		elseif ( $x == 'top' )
		{
			$x = 0;
		}
		elseif ( $x == 'bottom' )
		{
			$x = $this->height - $image->height;
		}
		
		// run image copy
		imagecopy( $this->image_context, $image->image_context, $x, $y, 0, 0, $image->width, $image->height );
		
		return $this;
	}

	/**
	 * fill the current image with an color
	 * you can pass an array with rgb or hex string
	 *
	 * @param mixed 		$color
	 * @return self
	 */
	public function fill_color( $color ) 
	{
		// parse the color
		$color = CCColor::create( $color );
		$color = imagecolorallocate( $this->image_context, $color->RGB[0], $color->RGB[1], $color->RGB[2] );

		// run image fill
		imagefill( $this->image_context, 0, 0, $color );
		
		return $this;
	}

	/**
	 * Blur the image using the gaussian blur.
	 *
	 * @param int 			$ratio
	 * @return self
	 */
	public function blur( $ratio = 5 ) 
	{
		for ($x=0; $x<$ratio; $x++) 
		{
			imagefilter($this->image_context, IMG_FILTER_GAUSSIAN_BLUR);
			//$gaussian = array(array(1.0, 2.0, 1.0), array(2.0, 1.0, 2.0), array(1.0, 2.0, 1.0));
			//imageconvolution($this->image_context, $gaussian, 16, 0);
		}
		
		return $this;
	}

	/**
	 * Get the average luminance of the image 
	 * 
	 * @param int 	$num_samples
	 * @return int	( 1-255 )
	 *
	 * @thanks to: http://stackoverflow.com/questions/596216/formula-to-determine-brightness-of-rgb-color
	 */
	public function get_luminance( $num_samples = 10 ) 
	{
		$x_step = (int) $this->width / $num_samples;
		$y_step = (int) $this->height / $num_samples;

		$total_lum = 0;
		$sample_no = 1;

		for ( $x=0; $x<$this->width; $x+=$x_step ) 
		{
			for ( $y=0; $y<$this->height; $y+=$y_step ) 
			{
				$rgb = imagecolorat($this->image_context, $x, $y);
				$r = ($rgb >> 16) & 0xFF;
				$g = ($rgb >> 8) & 0xFF;
				$b = $rgb & 0xFF;
				
				$lum = ($r+$r+$b+$g+$g+$g)/6;

				$total_lum += $lum;
				$sample_no++;
			}
		}
		
		return (int) ( $total_lum / $sample_no );
	}
}