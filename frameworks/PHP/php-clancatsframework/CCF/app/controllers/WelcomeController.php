<?php
/**
 * Welcome Controller
 ** 
 *
 * @package		ClanCatsFramework
 * @author		Mario Döring <mario@clancats.com>
 * @version		2.0
 * @copyright 	2010 - 2014 ClanCats GmbH
 *
 */
class WelcomeController extends CCViewController 
{	
	/**
	 * The default theme
	 * if you wish you can render this controller with a special theme
	 *
	 * @var string
	 */
	protected $_theme = null;
	
	/**
	 * the index function is just "function <controller_name>Index() {}" 
	 */
	public function action_index() 
	{
		$this->theme->topic = "Willkommen";
		
		$this->view = $this->theme->view( 'welcome', array(
			'runtime_name'	=> ClanCats::runtime( 'name' ),
			'environment'	=> ClanCats::environment(),
		));
	}
}