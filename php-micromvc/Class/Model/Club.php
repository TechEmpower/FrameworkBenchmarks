<?php
/**
 * Club Model
 *
 * @package		MicroMVC
 * @author		David Pennington
 * @copyright	(c) 2011 MicroMVC Framework
 * @license		http://micromvc.com/license
 ********************************** 80 Columns *********************************
 */
namespace Model;

class Club extends \Micro\ORM
{
	public static $table = 'club';
	public static $foreign_key = 'club_id';

	public static $has = array(
		'memberships' => '\Model\Membership'
	);

	public static $has_many_through = array(
		'students' => array(
			'club_id' => '\Model\Membership',
			'student_id' => '\Model\Student'
		),
	);
}
