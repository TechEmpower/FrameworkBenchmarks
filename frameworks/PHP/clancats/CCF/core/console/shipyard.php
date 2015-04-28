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
class shipyard extends \CCConsoleController 
{
	/**
	 * return an array with information about the script
	 */
	public function help() 
	{
		return array(
			'name'	=> 'Shipyard',
			'desc'	=> 'The shipyard is an helper that generates classes, configuration files, ships and other stuff for you.',
			'actions'	=> array(
				
				// class action
				'class'	=> array(
					'usage' => 'shipyard::class <classname>',
					'argumnets' => array(
						'classname' => 'The name of the class ( CCPath )'
					),
					'options' => array(
						'-extends' => 'The superclass that should be extended.',
						'-implements' => 'The interfaces that should be implemented.',
						'-no-init' => 'Don\'t generate the static init method.',
					),
				),
				
				// class action
				'model'	=> array(
					'usage' => 'shipyard::model <name> <db table>',
					'argumnets' => array(
						'name' => 'The name of the class ( CCPath )',
						'table' => 'The database table',
					),
					'options' => array(
						'-timestamps' => 'Does this model implement automatic time- stamps?',
					),
				),
				
				// controller action
				'controller' => array(
					'usage' => 'shipyard::controller <controllername>',
					'argumnets' => array(
						'controllername' => 'The name of the controller ( CCPath )'
					),
					'options' => array(
						'-view' => 'Is this a view controller?',
						'-actions' => 'The available actions in this controller.',
						'-no-events' => 'Don\'t generate the wake and sleep function.',
					),
				),
				
				// controller action
				'ship' => array(
					'usage' => 'shipyard::ship <name> <namespace>',
					'argumnets' => array(
						'name' => 'The name of the ship',
						'namespace' => 'The namespace the ship is going to operate from.',
					),
					'options' => array(
						'-no-namespace' => 'Create the ship without an own namespace.',
					),
				),
			),
		);
	}
	
	/**
	 * generate an class
	 *
	 * exmample:
	 * run shipyard::class <class>
	 * run shipyard::class <namespace>::<class>
	 *
	 * @param array 		$params 
	 * @return void
	 */
	public function action_class( $params ) 
	{	
		$options = \CCDataObject::assign( $params );
		
		$name = $options->get( 0, null );
		
		// get name if we dont have one
		while( !$name ) 
		{
			$name = $this->read( 'Please enter the class name: ' );
		}
		
		// try to resolve the path
		if ( !$path = \CCPath::classes( str_replace( '_', '/', $name ), EXT ) )
		{
			$this->error( 'Could not resolve the path. Check if the namespace is registered.' ); return;
		}
		
		// create the class
		$class = \CCShipyard::create( 'class', $name, $options->get( 'extends', null ), explode( ',', $options->get( 'implements', null ) ) );
		
		// add static init
		if ( !$options->get( 'no-init', false ) )
		{
			$class->add( 'function', '_init', 'public static', '//', "static class initialisation\n@return void" );
		}
				
		// check for overwrite
		if ( file_exists( $path ) )
		{
			if ( !$this->confirm( "The class already exists. Do you wish to overwrite it?", true ) ) 
			{
				return;
			}
		}
		
		// write file
		\CCFile::write( $path, $class->output() );
	}
	
	/**
	 * generate a model class
	 *
	 * exmample:
	 * run shipyard::model <class>
	 * run shipyard::model <class> <table>
	 * run shipyard::model <namespace>::<class>
	 *
	 * @param array 		$params 
	 * @return void
	 */
	public function action_model( $params ) 
	{	
		$options = \CCDataObject::assign( $params );
		
		// params
		$name = $options->get( 0, null );
		$table = $options->get( 1, null );
		
		// get name if we dont have one
		while( !$name ) 
		{
			$name = $this->read( 'Please enter the class name: ' );
		}
		
		// get table if we dont have one
		while( !$table ) 
		{
			$table = $this->read( 'Please enter the table name: ' );
		}
		
		// resolve the path
		if ( !$path = \CCPath::classes( str_replace( '_', '/', $name ), EXT ) )
		{
			$this->error( 'Could not resolve the path. Check if the namespace is registered.' ); return;
		}
		
		// create the class
		$model = \CCShipyard::create( 'dbmodel', $name, $table );
		
		// auto timestamps
		$model->timestamps( $options->get( 'timestamps', false ) );
		
		// check before
		if ( file_exists( $path ) )
		{
			if ( !CCCli::confirm( "The class already exists. Do you wish to overwrite it?", true ) ) 
			{
				return;
			}
		}
		
		// write file
		\CCFile::write( $path, $model->output() );
	}
	
	/**
	 * generate an controller
	 *
	 * exmample:
	 * run shipyard::controller <controller>
	 * run shipyard::controller <controller> <parent_class>
	 * run shipyard::controller <namespace>::<controller>
	 *
	 * @param array 		$params 
	 * @return void
	 */
	public function action_controller( $params ) 
	{	
		$options = \CCDataObject::assign( $params );
		
		$name = $options->get( 0, null );
		$parent = $options->get( 1, null );
		
		// get name if we dont have one
		while( !$name ) 
		{
			$name = $this->read( 'Please enter the controller name: ' );
		}
		
		// fix controller suffix
		if ( substr( $name, ( strlen( 'Controller' ) * -1 ) ) != 'Controller' )
		{
			$name .= 'Controller';
		}
		
		// try to resolve the path
		if ( !$path = \CCPath::controllers( str_replace( '_', '/', $name ), EXT ) )
		{
			$this->error( 'Could not resolve the path. Check if the namespace is registered.' ); return;
		}
		
		// parent
		if ( is_null( $parent ) )
		{
			$parent = '\\CCController';
		} 
		
		// view controller
		if ( $options->get( 'view', false ) )
		{
			$parent = '\\CCViewController';
		}
		
		// create the class
		$class = \CCShipyard::create( 'class', $name, $parent );
		
		// get the actions
		$actions = array( 'index' );
		
		if ( $options->get( 'actions', false ) )
		{
			$actions = array_merge( $actions, explode( ',', $options->get( 'actions' ) ) );
		}
		
		foreach( $actions as $action )
		{
			$action = trim( $action );
			
			$class->add( 'function', 'action_'.$action, 'protected', 'echo "'.$name.' '.$action.' action";', ucfirst($action)." action\n@return void|CCResponse" );
			$class->add( 'line', 2 );
		}
		
		// add static init
		if ( !$options->get( 'no-events', false ) )
		{
			$class->add( 'function', 'wake', 'protected', '//', "Controller wake\n@return void|CCResponse" );
			$class->add( 'line', 2 );
			$class->add( 'function', 'sleep', 'protected', '//', "Controller wake\n@return void" );
		}
						
		// check for overwrite
		if ( file_exists( $path ) )
		{
			if ( !$this->confirm( "The class already exists. Do you wish to overwrite it?", true ) ) 
			{
				return;
			}
		}
		
		// write file
		\CCFile::write( $path, $class->output() );
	}

	/**
	 * generate ships
	 *
	 * exmample:
	 * run shipyard::ship <name>
	 * run shipyard::ship <name> <namespace>
	 * run shipyard::ship <name> --no-namespace
	 *
	 * @param array 		$params 
	 * @return void
	 */
	public function action_ship( $params ) 
	{	
		$options = \CCDataObject::assign( $params );
		
		$name = $options->get( 0, null );
		$namespace = $options->get( 1, null );
		
		// get name if we dont have one
		while( !$name ) 
		{
			$name = CCCli::read( 'Please enter the ship name: ' );
		}
		
		// set namespace 
		if ( !$namespace ) 
		{
			$namespace = $name;
		}
		
		// no namespace?
		if ( $params['no-namespace'] ) 
		{
			$namespace = false;
		}
		
		// custom target
		$target = $options->get( 'target', ORBITPATH );
		
		if ( substr( $target, 0, 1 ) !== '/' )
		{
			$target = CCFPATH.$target;
		}
		if ( substr( $target, -1 ) !== '/' )
		{
			$target .= '/';
		}
		
		$target .= $name.'/';
		
		// check if the module is in our orbit path
		if ( is_dir( $target ) ) 
		{
			if ( !CCCli::confirm( "there is already a ship with this name. do you want to overwrite?", true ) ) 
			{
				return;
			}
		}
		
		// create the blueprint
		$defaults = \CCConfig::create( 'shipyard' );
		
		$blueprint = array(
			'name'			=> $name,
			'version'		=> $defaults->get( 'defaults.version' ),
			'description'	=> $defaults->get( 'defaults.description' ),
			'homepage'		=> $defaults->get( 'defaults.homepage' ),
			'keywords'		=> $defaults->get( 'defaults.keywords' ),
			'license'		=> $defaults->get( 'defaults.license' ),
			
			'authors'		=> $defaults->get( 'defaults.authors' ),
						
			'namespace'		=> $namespace,
		);
		
		// create file
		\CCJson::write( $target.'blueprint.json', $blueprint, true );
		
		
		$ship = \CCOrbit_Ship::blueprint( $blueprint, $target );
		
		// create event files
		if ( $namespace ) 
		{
			// create forge instance
			$forge = new \CCForge_Php( $namespace );
			
			// add header
			$forge->comment( $this->make_comment_header( $ship->name.' ship', array(
				'package'	=> $ship->name,
				'authors'	=> $ship->authors,
				'version'	=> $ship->version,
				'copyright'	=> \CCConfig::create( 'shipyard' )->get( 'defaults.copyright' ),
			)));
			
			// add class
			$forge->closure( 'class Ship extends \CCOrbit_Ship', function() {
				
				$forge = new \CCForge_Php;
				
				// add init function
				echo $forge->closure( 'public function wake()', '// Do stuff', 
					"initialize the ship\n\n".
					"@return void"
				);
				
				echo $forge->line(2);
				
				// add init function
				echo $forge->closure( 'public function install()', '// Do stuff', 
					"install the ship\n\n".
					"@return void"
				);
				
				echo $forge->line(2);
				
				// add init function
				echo $forge->closure( 'public function unsintall()', '// Do stuff', 
					"uninstall the ship\n\n".
					"@return void"
				);
			});
			
			\CCFile::write( $target.CCDIR_CLASS.'Ship'.EXT, $forge );
			
		} else {
			
			// create forge instance
			$forge = new \CCForge_Php();
			
			// add header
			$forge->comment( $this->make_comment_header( $ship->name, array(
				'package'	=> $ship->name,
				'authors'	=> $ship->authors,
				'version'	=> $ship->version,
				'copyright'	=> \CCConfig::create( 'shipyard' )->get( 'defaults.copyright' ),
			)));
			
			\CCFile::write( $target.'shipyard/wake'.EXT, $forge );
			\CCFile::write( $target.'shipyard/install'.EXT, $forge );
			\CCFile::write( $target.'shipyard/uninstall'.EXT, $forge );
		}
		
		// sucess
		CCCli::line( "'".$name."' succesfully created under: ".$target, 'green' );
	}
	
	/**
	 * generates an file header string
	 *
	 * @param string		$title
	 * @param array 		$data
	 * @return string
	 */
	public function make_comment_header( $title, $data = array() ) {
		
		// get author
		$authors = \CCArr::get( 'authors', $data, \CCConfig::create( 'shipyard' )->get( 'defaults.authors' ) );
		
		// author
		if ( is_array( $authors ) )
		{
			foreach( $authors as $person ) 
			{
				$author_str .= $person['name']." ";
				if ( array_key_exists( 'email', $person ) ) 
				{
					$author_str .= "<".$person['email'].">";
				}
				$author_str .= ", ";
			}
			$author_str = substr( $author_str, 0, -2 );
		}
		
		return "$title\n".
			"*\n".
			"\n".
			"@package       ".\CCArr::get( 'package', $data, \CCConfig::create( 'shipyard' )->get( 'defaults.package' ) )."\n".
			"@author        ".$author_str."\n".
			"@version       ".\CCArr::get( 'version', $data, \CCConfig::create( 'shipyard' )->get( 'defaults.version' ) )."\n".
			"@copyright     ".\CCArr::get( 'copyright', $data, \CCConfig::create( 'shipyard' )->get( 'defaults.copyright' ) )."\n";
	}
}