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

/**
 * NOTICE:
 *
 * If you need to make modifications to the default configuration, copy
 * this file to your app/config folder, and make them in there.
 *
 * This will allow you to upgrade fuel without losing your custom config.
 */


return array(
	/**
	 * The driver to be used. Currently gd, imagemagick or imagick
	 */
	'driver' => 'gd',

	/**
	 * Sets the background color of the image.
	 *
	 * Set to null for a transparent background.
	 */
	'bgcolor' => null,

	/**
	 * Sets the transparency of any watermark added to the image.
	 */
	'watermark_alpha' => 75,

	/**
	 * The quality of the image being saved or output, if the format supports it.
	 */
	'quality' => 100,

	/**
	 * Lets you use a default container for images. Override by Image::output('png') or Image::save('file.png')
	 *
	 * Examples: png, bmp, jpeg, ...
	 */
	'filetype' => null,

	/**
	 * The install location of the imagemagick executables.
	 */
	'imagemagick_dir' => '/usr/bin/',

	/**
	 * Temporary directory to store image files in that are being edited.
	 */
	'temp_dir' => APPPATH.'tmp'.DS,

	/**
	 * The string of text to append to the image.
	 */
	'temp_append' => 'fuelimage_',

	/**
	 * Sets if the queue should be cleared after a save(), save_pa(), or output().
	 */
	'clear_queue' => true,

	/**
	 * Determines whether to automatically reload the image (false) or keep the changes (true) when saving or outputting.
	 */
	'persistence' => false,

	/**
	 * Used to debug the class, defaults to false.
	 */
	'debug' => false,

	/**
	 * These presets allow you to call controlled manipulations.
	 */
	'presets' => array(

		/**
		 * This shows an example of how to add preset manipulations
		 * to an image.
		 *
		 * Note that config values here override the current configuration.
		 *
		 * Driver cannot be changed in here.
		 */
		'example' => array(
			'quality' => 100,
			'bgcolor' => null,
			'actions' => array(
				array('crop_resize', 200, 200),
				array('border', 20, "#f00"),
				array('rounded', 10),
				array('output', 'png')
			)
		)
	)
);


