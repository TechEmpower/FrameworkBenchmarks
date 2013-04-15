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
	 * Defaults used for formatting options
	 *
	 * @var   array
	 */
	'formatting' => array(
		// Num::format_phone()
		'phone' => '(000) 000-0000',
		// Num::smart_format_phone()
		'smart_phone' => array(
			7  => '000-0000',
			10 => '(000) 000-0000',
			11 => '0 (000) 000-0000',
		),
		// Num::format_exp()
		'exp' => '00-00',
		// Num::mask_credit_card()
		'credit_card' => '**** **** **** 0000',
	),

);

/* End of file config/num.php */
