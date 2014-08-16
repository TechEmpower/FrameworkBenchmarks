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

namespace Orm;

abstract class Relation
{
	/**
	 * @var  string  name of the relationship in the model_from
	 */
	protected $name;

	/**
	 * @var  Model  classname of the parent model
	 */
	protected $model_from;

	/**
	 * @var  string  classname of the related model
	 */
	protected $model_to;

	/**
	 * @var  string  primary key of parent model
	 */
	protected $key_from = array('id');

	/**
	 * @var  string  foreign key in related model
	 */
	protected $key_to = array();

	/**
	 * @var  array  where & order_by conditions for loading this relation
	 */
	protected $conditions = array();

	/**
	 * @var  bool  whether it's a single object or multiple
	 */
	protected $singular = false;

	/**
	 * @var  bool  whether saving this one's model_from should cascade to save model_to
	 */
	protected $cascade_save = true;

	/**
	 * @var  bool  whether deleting this one's model_from should cascade to delete model_to
	 */
	protected $cascade_delete = false;

	/**
	 * Configures the relationship
	 *
	 * @param  string  the model that initiates the relationship
	 * @param  string  name of the relationship
	 * @param  array   config values like model_to classname, key_from & key_to
	 */
	abstract public function __construct($from, $name, array $config);

	/**
	 * Should get the objects related to the given object by this relation
	 *
	 * @param   Model
	 * @return  object|array
	 */
	abstract public function get(Model $from);

	/**
	 * Should get the properties as associative array with alias => property, the table alias is
	 * given to be included with the property
	 *
	 * @param   string
	 * @return  array
	 */
	public function select($table)
	{
		$props = call_user_func(array($this->model_to, 'properties'));
		$i = 0;
		$properties = array();
		foreach ($props as $pk => $pv)
		{
			$properties[] = array($table.'.'.$pk, $table.'_c'.$i);
			$i++;
		}

		return $properties;
	}

	/**
	 * Returns tables to join and fields to select with optional additional settings like order/where
	 *
	 * @param   string  alias for the from table
	 * @param   string  alias for the to table
	 * @return  array
	 */
	abstract public function join($alias_from, $rel_name, $alias_to);

	/**
	 * Saves the current relationships and may cascade saving to model_to instances
	 *
	 * @param  Model        instance of model_from
	 * @param  array|Model  single or multiple model instances to save
	 * @param  bool         whether the model_from has been saved already
	 * @param  null|bool    either uses default setting (null) or forces when true or prevents when false
	 * @todo   make abstract
	 */
	abstract public function save($model_from, $model_to, $original_model_id, $parent_saved, $cascade);

	/**
	 * Takes the current relations and attempts to delete them when cascading is allowed or forced
	 *
	 * @param  Model        instance of model_from
	 * @param  array|Model  single or multiple model instances to delete
	 * @param  bool         whether the model_from has been saved already
	 * @param  null|bool    either uses default setting (null) or forces when true or prevents when false
	 * @todo   make abstract
	 */
	abstract public function delete($model_from, $model_to, $parent_deleted, $cascade);

	/**
	 * Allow outside access to protected properties
	 *
	 * @param  $property
	 */
	public function __get($property)
	{
		if (strncmp($property, '_', 1) == 0 or ! property_exists($this, $property))
		{
			throw new \FuelException('Invalid relation property: '.$property);
		}

		return $this->{$property};
	}
}
