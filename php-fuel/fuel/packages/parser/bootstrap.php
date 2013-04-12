<?php
/**
 * Fuel
 *
 * Fuel is a fast, lightweight, community driven PHP5 framework.
 *
 * @package    Fuel
 * @version    1.5
 * @author     Fuel Development Team
 * @license    MIT License
 * @copyright  2010 - 2013 Fuel Development Team
 * @link       http://fuelphp.com
 */

Autoloader::add_core_namespace('Parser');

Autoloader::add_classes(array(
	'Parser\\View'             => __DIR__.'/classes/view.php',
	'Parser\\View_Dwoo'        => __DIR__.'/classes/view/dwoo.php',
	'Parser\\View_Mustache'    => __DIR__.'/classes/view/mustache.php',
	'Parser\\View_Markdown'    => __DIR__.'/classes/view/markdown.php',
	'Parser\\View_SimpleTags'  => __DIR__.'/classes/view/simpletags.php',
	'Parser\\View_Twig'        => __DIR__.'/classes/view/twig.php',
	'Parser\\Twig_Fuel_Extension' => __DIR__.'/classes/twig/fuel/extension.php',
	'Parser\\View_Jade'        => __DIR__.'/classes/view/jade.php',
	'Parser\\View_Haml'        => __DIR__.'/classes/view/haml.php',
	'Parser\\View_Smarty'      => __DIR__.'/classes/view/smarty.php',
	'Parser\\View_Phptal'      => __DIR__.'/classes/view/phptal.php',
));


/* End of file bootstrap.php */
