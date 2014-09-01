<?php namespace Core;
/**
 * View Controller 
 ** 
 *
 * @package		ClanCatsFramework
 * @author		Mario Döring <mario@clancats.com>
 * @version		2.0
 * @copyright 	2010 - 2014 ClanCats GmbH
 *
 */
class CCViewController extends CCController 
{
	/**
	 * The default theme
	 * if you wish you can render this controller with a special theme
	 *
	 * @var string
	 */
	protected $_theme = null;
	
	/**
	 * The default layout
	 * define another default layout than the one that the theme defines.
	 *
	 * @var string
	 */
	protected $_layout = null;
	
	/**
	 * The current view object
	 *
	 * @var CCView
	 */
	protected $view = null;
	
	/**
	 * The current theme object
	 *
	 * @var CCTheme
	 */
	protected $theme = null;
	
	/**
	 * just render the view ignore the layout
	 *
	 * @var bool
	 */
	protected $modal = false;
	
	/**
	 * Controller constructor
	 * Creates the theme instance and assign it to curret view globals
	 *
	 * @return void
	 */
	public function __construct()
	{
		if ( is_null( $this->_theme ) ) 
		{
			$this->_theme = ClanCats::$config->get( 'viewcontroller.theme.default' );
		}
		
		if ( is_null( $this->_layout ) ) 
		{
			$this->_layout = ClanCats::$config->get( 'viewcontroller.theme.default_layout' );
		}
		
		$theme_class = "\\".$this->_theme."\\Theme";
		$this->theme = $theme_class::create( $this->_layout );
		
		// assign the theme to the view before render
		CCView::share( 'theme', $this->theme );
	}
	
	/**
	 * When a controller gives us a string make a response out of it
	 * 
	 * @param string
	 * @return CCResponse
	 */
	protected function _respond( $string )
	{
		// if no view isset create one with the output buffer
		if ( is_null( $this->view ) ) 
		{
			$this->theme->content = $string;
		} else {
			$this->theme->content = $this->view->render();
		}
		
		// dont render the theme? Or does the request force render in modal?
		if ( CCRequest::current()->args->force_modal || $this->modal ) 
		{
			return CCResponse::create( $this->theme->content, $this->status );
		}
		
		// render theme and return response
		return CCResponse::create( $this->theme->render(), $this->status );
	}
}