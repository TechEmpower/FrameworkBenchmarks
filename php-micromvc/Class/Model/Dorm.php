<?php
/**
 * Dorm Model
 *
 * @package		MicroMVC
 * @author		David Pennington
 * @copyright	(c) 2011 MicroMVC Framework
 * @license		http://micromvc.com/license
 ********************************** 80 Columns *********************************
 */
namespace Model;

class Dorm extends \Micro\ORM
{
	public static $table = 'dorm';
	public static $foreign_key = 'dorm_id';

	public static $has = array(
		'students' => '\Model\Student',
	);

}
