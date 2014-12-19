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
class orbit extends \CCConsoleController {
	
	/**
	 * return an array with information about the script
	 */
	public function help() 
	{
		return array(
			'name'	=> 'Orbit',
			'desc'	=> 'CCF module / plugin managment, we call them ships.',
			'actions'	=> array(
				'install'	=> 'install a ship. usage: install <ship path>',
				'uninstall'	=> 'remove a ship. usage: uninstall <ship path>',
			),
		);
	}
	
	/**
	 * install an orbit module
	 *
	 * @param array 		$params 
	 */
	public function action_install( $params ) {
		
		$path = $params[0];
		
		// get target directory
		$target_dir = \CCArr::get( 'target', $params, ORBITPATH );
		
		if ( empty( $path ) ) {
			CCCli::line( 'no ship path given.', 'red' ); return;
		}
		
		/*
		 * direct install if starting with /
		 */
		if ( substr( $path, 0, 1 ) == '/' ) {
			
			// fix path
			if ( substr( $path, -1 ) != '/' ) {
				$path .= '/';
			}
			
			// is directory
			if ( !is_dir( $path ) ) {
				CCCli::line( 'could not find a ship at path: '.$path, 'red' ); return;
			}
			
			// define the target
			$target = $target_dir.basename( $path ).'/';
			
			// check if we already have an directory with the same name
			if ( $target != $path && is_dir( $target ) ) {
				if ( !CCCli::confirm( "there is already a ship with this name. do you want to overwrite?", true ) ) {
					return;
				}
			}
			
			// are ya serius..
			if ( !CCCli::confirm( "are you sure you want to install this ship?", true ) ) {
				return;
			}
			
			// move the directory
			if ( $target != $path ) {
				rename( $path, $target );
			}

			// run the installer
			try {
				\CCOrbit::install( $target );
			}
			catch ( \Exception $e ) {
				CCCli::line( $e->getMessage(), 'red' ); 
				CCCli::line( 'ship installation failure.', 'red' ); return;
			}
			
			// we are done
			CCCli::line( 'ship installation succeeded', 'green' );
			
			return;
		}
		
		// check if the module is in our orbit path
		if ( is_dir( ORBITPATH.$path ) ) {
			
			// there is a ship yay
			CCCli::line( 'found ship at path: '.ORBITPATH.$path, 'green' );
			
			return static::action_install( array( ORBITPATH.$path, 'target' => \CCArr::get( 'target', $params ) ) );
		}
		
		// check if the module is in CCF dir
		if ( is_dir( CCROOT.$path ) ) {
			
			// there is a ship yay
			CCCli::line( 'found ship at path: '.CCROOT.$path, 'green' );
			
			return static::action_install( array( CCROOT.$path, 'target' => \CCArr::get( 'target', $params ) ) );
		}

		// search the repository for this ship
		CCCli::line( 'searching the repositories for: '.$path.' ...', 'cyan' );
	}
	
	/**
	 * uninstall an orbit module
	 *
	 * @param array 		$params 
	 */
	public function action_uninstall( $params ) {
		
		$path = $params[0]; 
		
		if ( empty( $path ) ) {
			CCCli::line( 'no ship path given.', 'red' ); return;
		}
		
		/*
		 * direct install if starting with /
		 */
		if ( substr( $path, 0, 1 ) == '/' ) {
			
			// fix path
			if ( substr( $path, -1 ) != '/' ) {
				$path .= '/';
			}
			
			// is directory
			if ( !is_dir( $path ) ) {
				CCCli::line( 'could not find a ship at path: '.$path, 'red' ); return;
			}
			
			// are ya serius..
			if ( !CCCli::confirm( "are you sure you want to uninstall this ship?", true ) ) {
				return;
			}
			
			// run the uninstaller
			try {
				\CCOrbit::uninstall( $path );
			}
			catch ( \Exception $e ) {
				CCCli::line( $e->getMessage(), 'red' ); 
				CCCli::line( 'ship destroying failure.', 'red' ); return;
			}
			
			// also remove the direcoty?
			if ( CCCli::confirm( "do you also wish to remove the ship files?", true ) ) {
				\CCFile::delete( $path );
			}
			
			// we are done
			CCCli::line( 'ship destroyed!', 'green' );
			
			return;
		}
		
		// check if the module is in our orbit path
		if ( is_dir( ORBITPATH.$path ) ) {
			
			// there is a ship yay
			CCCli::line( 'found ship at path: '.ORBITPATH.$path, 'green' );
			
			return static::action_uninstall( array( ORBITPATH.$path ) );
		}
	
		// nothing to do here
		CCCli::line( 'could not find a ship at this path or name.', 'red' ); return;
	}
}