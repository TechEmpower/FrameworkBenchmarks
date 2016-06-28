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

	/*
	 * Not Found and internal Server Error
	 */
	'#404'	=> 'Error@404',
	'#500'	=> 'Error@500',

	/**
	 * Plaintext response benchmark
	 */
	'plaintext' => function()
	{
		$response = CCResponse::create("Hello, World!", 200);
		$response->header("Content-Type", "text/plain; charset=UTF-8");
		return $response;
 	},
       	
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
	 * DB response benchmark
	 */
	'db' => function() 
	{		
		$world = DB::select( 'World' )->find( mt_rand(1, 10000) );
		$world->id = intval($world->id);
		$world->randomNumber = intval($world->randomNumber);
		return CCResponse::create( json_encode( $world), 200 )
			->header( 'Content-Type', 'application/json' );
	},
	
	/**
	 * Qeuries response benchmark
	 */
	'queries' => function() 
	{		
		$queries = CCIn::get( 'queries', 1 );
		if ($queries < 1) {
			$queries = 1;
	        }  
	        if ($queries > 500) {
			$queries = 500;
		}	
                $worlds = array();
		
		for($i = 0; $i < $queries; ++$i) 
		{
			$world = DB::select( 'World' )->find( mt_rand(1, 10000) );
			$world->id = intval($world->id);
			$world->randomNumber = intval($world->randomNumber);
			$worlds[] = $world;
		}
		return CCResponse::create( json_encode( $worlds), 200 )
			->header( 'Content-Type', 'application/json' );
	},

	/**
	 * Updates response benchmark
	 */
	'updates' => function() 
	{		
		$queries = CCIn::get( 'queries', 1 );
		if ($queries < 1) {
			$queries = 1;
	        }  
	        if ($queries > 500) {
			$queries = 500;
		}	
                $worlds = array();
		
		for($i = 0; $i < $queries; ++$i) 
		{
			$id = mt_rand(1, 10000);
			DB::update( 'World' )
				->set( 'randomNumber', mt_rand(1, 10000) )
				->where( 'id', $id )
				->run(); 
			$world = DB::select( 'World' )->find( $id );
			$world->id = intval($world->id);
			$world->randomNumber = intval($world->randomNumber);
			$worlds[] = $world;
		}
		return CCResponse::create( json_encode( $worlds), 200 )
			->header( 'Content-Type', 'application/json' );
	},
	
	'fortunes' => 'Bench@fortunes',
);
