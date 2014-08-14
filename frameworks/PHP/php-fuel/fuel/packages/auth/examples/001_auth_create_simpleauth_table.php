<?php

namespace Fuel\Migrations;

class Auth_Create_Simpleauth_Table
{

	function up()
	{
		// get the configured table name
		$table = \Config::get('simpleauth.table_name', 'users');

		// table users
		\DBUtil::create_table($table, array(
			'id' => array('type' => 'int', 'constraint' => 11, 'auto_increment' => true),
			'username' => array('type' => 'varchar', 'constraint' => 50),
			'password' => array('type' => 'varchar', 'constraint' => 255),
			'group' => array('type' => 'int', 'constraint' => 11, 'default' => 1),
			'email' => array('type' => 'varchar', 'constraint' => 255),
			'last_login' => array('type' => 'varchar', 'constraint' => 25),
			'login_hash' => array('type' => 'varchar', 'constraint' => 255),
			'profile_fields' => array('type' => 'text'),
			'created_at' => array('type' => 'int', 'constraint' => 11, 'default' => 0),
		), array('id'));

		// add a unique index on username and email
		\DBUtil::create_index('users', array('username', 'email'), 'username', 'UNIQUE');
	}

	function down()
	{
		// get the configured table name
		$table = \Config::get('simpleauth.table_name', 'users');

		// drop the users table
		\DBUtil::drop_table($table);
	}
}
