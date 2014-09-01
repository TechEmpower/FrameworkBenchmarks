<?php 
/*
 *---------------------------------------------------------------
 * Router map configuration
 *---------------------------------------------------------------
 *
 * You can use this configuration to set you default application
 * routes.
 */
return array(
	
	/**
	 * JSON response benchmark
	 */
	'json' => function() 
	{
		return CCResponse::create( json_encode(
			array('message' => 'Hello, World!')
		), 200 )->header( 'Content-Type', 'application/json' );
	},
	
	/**
	 * JSON response benchmark
	 */
	'db' => function() 
	{		
		$queries = CCIn::get( 'queries', 1 );
		$worlds = array();
		
		for($i = 0; $i < $queries; ++$i) 
		{
			$worlds[] = DB::select( 'World' )->find( mt_rand(1, 10000) );
		}
		
		return CCResponse::create( json_encode( $worlds ) )
			->header( 'Content-Type', 'application/json' );
	},
	
	'fortunes' => 'Bench@fortunes',
);
