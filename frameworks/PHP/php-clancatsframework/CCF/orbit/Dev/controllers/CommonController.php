<?php namespace Dev;
/**
 * Development index controller
 **
 * 
 * @package       Dev
 * @author        Mario Döring <mario@clancats.com>
 * @version       1.0
 * @copyright     2010 - 2014 ClanCats GmbH
 *
 */
class CommonController extends \CCViewController 
{
	
	/**
	 * show php info 
	 */
	public function action_phpinfo() {
		
		// set topic
		$this->theme->topic = "php info";
		
		// get php info in a string
		ob_start(); phpinfo(); $pinfo = ob_get_contents(); ob_end_clean();
		
		// get only the body
		$pinfo = preg_replace( '%^.*<body>(.*)</body>.*$%ms','$1',$pinfo);
		
		// add table class
		$pinfo = str_replace( '<table', '<table class="table table-bordered"', $pinfo );
		
		// better headers
		$pinfo = \CCStr::replace( $pinfo, array(
			'</h2>' => '</h2><hr>'
		));
		
		echo $pinfo;
	}
	
	/**
	 * show stylesheet overview
	 */
	public function action_css() {
		
		// set topic
		$this->theme->topic = "CSS Overview";
		
		// load the css overview
		$this->view = CCView::create( 'development/cssView' );
	}
}