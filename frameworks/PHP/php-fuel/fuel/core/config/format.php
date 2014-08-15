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
	'csv' => array(
		'delimiter' => ',',
		'enclosure' => '"',
		'newline'   => "\n",
		'regex_newline'   => '\n',
		'escape'    => '\\',
	),
	'xml' => array(
		'basenode' => 'xml',
	),
);
