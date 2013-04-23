<?php
/**
 * Fuel is a fast, lightweight, community driven PHP5 framework.
 *
 * @package    Fuel
 * @version    1.5
 * @author     Fuel Development Team
 * @license    MIT License
 * @copyright  2010 - 2013 Fuel Development Team
 * @link       http://fuelphp.com
 */


Autoloader::add_classes(array(
	'Orm\\Model'        => __DIR__.'/classes/model.php',
	'Orm\\Query'        => __DIR__.'/classes/query.php',
	'Orm\\BelongsTo'    => __DIR__.'/classes/belongsto.php',
	'Orm\\HasMany'      => __DIR__.'/classes/hasmany.php',
	'Orm\\HasOne'       => __DIR__.'/classes/hasone.php',
	'Orm\\ManyMany'     => __DIR__.'/classes/manymany.php',
	'Orm\\Relation'     => __DIR__.'/classes/relation.php',

	//Speclised models
	'Orm\\Model_Soft'      => __DIR__.'/classes/model/soft.php',

	// Observers
	'Orm\\Observer'             => __DIR__.'/classes/observer.php',
	'Orm\\Observer_CreatedAt'   => __DIR__.'/classes/observer/createdat.php',
	'Orm\\Observer_Typing'      => __DIR__.'/classes/observer/typing.php',
	'Orm\\Observer_UpdatedAt'   => __DIR__.'/classes/observer/updatedat.php',
	'Orm\\Observer_Validation'  => __DIR__.'/classes/observer/validation.php',
	'Orm\\Observer_Self'        => __DIR__.'/classes/observer/self.php',
	'Orm\\Observer_Slug'        => __DIR__.'/classes/observer/slug.php',

	// Exceptions
	'Orm\\RecordNotFound'      => __DIR__.'/classes/model.php',
	'Orm\\FrozenObject'        => __DIR__.'/classes/model.php',
	'Orm\\InvalidContentType'  => __DIR__.'/classes/observer/typing.php',
	'Orm\\ValidationFailed'    => __DIR__.'/classes/observer/validation.php',
	'Orm\\RelationNotSoft' => __DIR__.'/classes/model/soft.php',
));
