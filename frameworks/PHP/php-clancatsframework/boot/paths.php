<?php 
/*
 *---------------------------------------------------------------
 * Framework paths
 *---------------------------------------------------------------
 *
 * This is a much more beautiful way then using defines in the 
 * framework init. Also it allows us for example running 
 * multiple apps using just one core.
 */
return array(
	
	/*
	 *---------------------------------------------------------------
	 *  public path = PUBLICPATH
	 *---------------------------------------------------------------
	 *
	 * The public directory wich should be accessable through your
	 * web server on your browser.
	 */
	'public'			=> CCROOT.'public/',
	
	/*
	 *---------------------------------------------------------------
 	 *  CCF path = CCFPATH
	 *---------------------------------------------------------------
	 *
	 * The path to the framework directory. In the default use case
	 * this conatins the app, core, orbit and vendor. But also 
	 * the storage uses this path to set his default store direcotry.
	 */
	'ccf'			=> CCROOT.'CCF/',
	
	/*
	 *---------------------------------------------------------------
	 *  App path = APPPATH
	 *---------------------------------------------------------------
	 *
	 * The app directory contains the main application. This define 
	 * can be useful if you like to switch between apps. 
	 */
	'app'			=> CCROOT.'CCF/app/',
	
	/*
	 *---------------------------------------------------------------
	 *  Core path = COREPATH
	 *---------------------------------------------------------------
	 *
	 * The core directory contains of course the CCF core.
	 * Changing this can be useful if you have multiple 
	 * installations that should use the same core.
	 *
	 * @todo: take this back to CCF/core/ when the composer installer
	 *        issue is fixed.
	 */
	'core'			=> CCROOT.'CCF/core/',
	
	/*
	 *---------------------------------------------------------------
	 *  Orbit path = ORBITPATH
	 *---------------------------------------------------------------
	 *
	 * The orbit directory contains the most installed ships.
	 * Changing this can be useful if you have multiple 
	 * installations that should use the same orbit packages.
	 */
	'orbit'			=> CCROOT.'CCF/orbit/',
	
	/*
	 *---------------------------------------------------------------
	 *  Vendor path = VENDORPATH
	 *---------------------------------------------------------------
	 *
	 * The vendor directory contains the composer packages.
	 */
	'vendor'			=> CCROOT.'CCF/vendor/',
);