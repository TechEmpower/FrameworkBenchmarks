<?php
/**
 * Membership Model
 *
 * @package		MicroMVC
 * @author		David Pennington
 * @copyright	(c) 2011 MicroMVC Framework
 * @license		http://micromvc.com/license
 ********************************** 80 Columns *********************************
 */
namespace Model;

class Membership extends \Micro\ORM
{
	public static $table = 'membership';
	public static $foreign_key = 'membership_id';

	public static $belongs_to = array(
		'student' => '\Model\Student',
		'club' => '\Model\Club',
	);
}
