<?php
/**
 * Car Model
 *
 * @package		MicroMVC
 * @author		David Pennington
 * @copyright	(c) 2011 MicroMVC Framework
 * @license		http://micromvc.com/license
 ********************************** 80 Columns *********************************
 */
namespace Model;

class Car extends \Micro\ORM
{
	public static $table = 'car';
	public static $foreign_key = 'car_id';

	public static $belongs_to = array(
		'student' => '\Model\Student',
	);

}
