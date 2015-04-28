<?php namespace Auth;
/**
 * Group
 **
 * 
 * @package		ClanCatsFramework
 * @author		Mario Döring <mario@clancats.com>
 * @version		2.0
 * @copyright 	2010 - 2014 ClanCats GmbH
 *
 */
class Group extends \DB\Model
{
	/**
	 * The database table name
	 * 
	 * @var string
	 */
	protected static $_table = 'auth_groups';
	
	/**
	 * Let the model automatically set created_at and modified_at 
	 *
	 * @var bool
	 */
	protected static $_timestamps = true;
	
	/**
	 * The  default properties
	 * 
	 * @var string
	 */
	protected static $_defaults = array(
		'id',
		'name' => array( 'string', '' ),
		'created_at' => array( 'int', 0 ),
		'modified_at' => array( 'int', 0 )
	);
}
