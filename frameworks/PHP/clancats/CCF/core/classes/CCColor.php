<?php namespace Core;
/**
 * ClanCats color helper
 *
 * @package 			ClanCats-Framework
 * @author     		Mario Döring <mariodoering@me.com>
 * @version 			0.5
 * @copyright 		2010 - 2013 ClanCats GmbH 
 *
 */
class CCColor 
{
	/**
	 * parse an color 
	 * 
	 * @param mixed 		$color 
	 */
	public static function create( $color ) 
	{
		// our return
		$rgb = array();

		if ( is_array( $color ) ) 
		{
			$color = array_values( $color );
			$rgb[0] = $color[0];
			$rgb[1] = $color[1];
			$rgb[2] = $color[2];
		}
		// parse hex color 
		elseif ( is_string( $color ) && substr( $color, 0, 1 ) == '#' ) 
		{
			$color = substr( $color, 1 );

			if( strlen( $color ) == 3 ) 
			{
				$rgb[0] = hexdec( substr( $color, 0, 1 ) . substr( $color, 0, 1 ) );
				$rgb[1] = hexdec( substr( $color, 1, 1 ) . substr( $color, 1, 1 ) );
				$rgb[2] = hexdec( substr( $color, 2, 1 ) . substr( $color, 2, 1 ) );
			}
			elseif( strlen( $color ) == 6 ) 
			{
				$rgb[0] = hexdec( substr( $color, 0, 2 ) );
				$rgb[1] = hexdec( substr( $color, 2, 2 ) );
				$rgb[2] = hexdec( substr( $color, 4, 2 ) );
			}
		}
		// could not be parsed 
		else 
		{
			return false;
		}

		return new static( $rgb );
	}

	/*
	 * our color holder
	 */ 
	public $RGB = array( 0, 0, 0 );

	/**
	 * init a new color instance
	 */
	public function __construct( $rgb ) 
	{
		$this->RGB = $rgb;
	}
}