<?php 
/*
 *---------------------------------------------------------------
 * Environment detector
 *---------------------------------------------------------------
 *
 * This is the environment detector, you can return an string or
 * an array. 
 *
 * passing an array:
 *     If you pass an array to the detector he will try to set
 *     the environment using an hostname pattern example:
 *     'local.*' 	=> 'development',
 *     'test.*'		=> 'staging',
 *     ':all'		=> 'production',
 *
 * passing an string:
 *     Will set the environment directly example:
 *	   return $_SERVER[ 'CCF_ENV' ]
 *
 */
return array(
	
	/*
	 * probably an local environment for example:
	 *     localhost
	 *     local.example.com
	 */
	'local*'		=> 'development',
	
	/*
	 * probably an testing environment for example:
	 *     test.example.com
	 */
	'test.*'		=> 'test',
	
	/*
	 * everything else for example:
	 *     www.example.com
	 *     clancats.com
	 */
	':all'		=> 'production',
);