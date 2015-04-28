<?php namespace CCConsole; use CCCli;
/**
 * Console Controller 
 * run a application script
 ** 
 *
 * @package		ClanCatsFramework
 * @author		Mario Döring <mario@clancats.com>
 * @version		2.0
 * @copyright 	2010 - 2014 ClanCats GmbH
 *
 */
class doctor extends \CCConsoleController {

	/**
	 * return an array with information about the script
	 */
	public function help() 
	{
		return array(
			'name'	=> 'Dr. CCF',
			'desc'	=> 'Can do several operations on the application.',
			'actions'	=> array(
				
				'permissions'	=> array(
					'info' => 'Will check the write permissions of the directories and tries to fix them if needed.',
					'usage' => 'doctor::permissions',
				),
				
				'security_key'=> array(
					'info' => 'Generates a new security salt if needed.',
					'usage' => 'shipyard::security_key',
				),
			),
		);
	}
	
	/**
	 * Try to generate a security key in the main config file
	 *
	 * @param array 		$params 
	 */
	public function action_security_key( $params )
	{
		$path = \CCPath::config( 'main.config'.EXT );
		
		// Check if the file exists
		if ( !file_exists( $path ) )
		{
			$this->error( 'Could not find main configuration file.' ); return;
		}
		
		// Now try to replace the placeholder with 
		// an new generated key
		$data = \CCFile::read( $path );
		
		if ( strpos( $data, '{{security salt here}}' ) === false )
		{
			$this->error( 'The key has already been generated or set.' ); return;
		}
		
		$data = str_replace( '{{security salt here}}', \CCStr::random( 32, 'password' ), $data );
		
		// write the data back down
		\CCFile::write( $path, $data );
		
		$this->success( 'The key has been generated.' );
	}
	
	/**
	 * install an orbit module
	 *
	 * @param array 		$params 
	 */
	public function action_permissions( $params ) 
	{
		$folders = \CCEvent::fire( 'ccdoctor.permissions' );
		
		if ( !is_array( $folders ) )
		{
			$folders = array();
		}
		
		// add storage directories
		foreach( \ClanCats::$config->get( 'storage.paths' ) as $folder )
		{
			$folders[] = $folder;
		}
		
		foreach( $folders as $folder )
		{
			$display_folder = \CCStr::replace( $folder, array( CCROOT => '' ) );
			
			// create directory if not existing
			if ( !is_dir( $folder ) ) 
			{
				if ( !mkdir( $folder, 0755, true ) ) 
				{
					$this->error( "doctor could not create folder at: {$display_folder}" );
				}
			}
			
			// check permissions
			$perm = substr( decoct( fileperms( $folder ) ), 2 );
		
			if ( $perm < 755 )
			{
				CCCli::line( CCCli::color( $perm, 'red' ).
					' - '.
					$display_folder.
					' fixing with '.CCCli::color( '755', 'green' ) 
				);
				if ( !chmod( $folder, 0755 ) )
				{
					CCCli::line( "doctor - is not able to change permissions for: {$display_folder}", 'red' );
				}
			}
			elseif ( $perm == 777 )
			{
				CCCli::line( CCCli::color( $perm, 'yellow' ).
					' - '.
					$display_folder.
					' warning! this can be dangerous.'
				);
			}
			else 
			{
				$this->success( '- '.$display_folder, $perm );
			}
		}
	}
}