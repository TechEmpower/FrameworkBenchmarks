<?php 
/*
 *---------------------------------------------------------------
 * ClanCatsFramework runner
 *---------------------------------------------------------------
 *
 * This file just loads CCF and all needed resources to run the 
 * php unit tests. PHPUnit is a really elegant way to make sure 
 * that everything works how it should.
 *
 *---------------------------------------------------------------
 * Require CCF
 *---------------------------------------------------------------
 *
 * load the framework file wich wil initialize CCF. 
 */
require_once __DIR__."/clancatsapp/framework.php";

/*
 * execute the main request
 * The main request contains the params of the http header
 */
$response = CCRequest::uri( CCServer::uri() )
    ->perform()
    ->response();

/*
 * "send" means actaully printing the response.
 * If the secound parameter is true the response will 
 * also set headers
 */
$response->send( true );
