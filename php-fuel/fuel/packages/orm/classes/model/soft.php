<?php

namespace Orm;

class RelationNotSoft extends \Exception
{

}

/**
 * Defines a model that can be "soft" deleted. A timestamp is used to indicate
 * that the data has been deleted but the data itself is not removed from the
 * database.
 *
 * @author Steve "Uru" West <uruwolf@gmail.com>
 */
class Model_Soft extends Model
{

	/**
	 * Default column name that contains the deleted timestamp
	 * @var string
	 */
	protected static $_default_field_name = 'deleted_at';

	/**
	 * Default value for if a mysql timestamp should be used.
	 * @var boolean
	 */
	protected static $_default_mysql_timestamp = true;

	/**
	 * Contains cached soft delete properties.
	 * @var array
	 */
	protected static $_soft_delete_cached = array();

	protected static $_disable_filter = array();

	/**
	 * Gets the soft delete properties.
	 * Mostly stolen from the parent class properties() function
	 *
	 * @return array
	 */
	public static function soft_delete_properties()
	{
		$class = get_called_class();

		// If already determined
		if (array_key_exists($class, static::$_soft_delete_cached))
		{
			return static::$_soft_delete_cached[$class];
		}

		$properties = array();

		// Try to grab the properties from the class...
		if (property_exists($class, '_soft_delete'))
		{
			//Load up the info
			$properties = static::$_soft_delete;
		}

		// cache the properties for next usage
		static::$_soft_delete_cached[$class] = $properties;

		return static::$_soft_delete_cached[$class];
	}

	/**
	 * Disables filtering of deleted entries.
	 */
	public static function disable_filter()
	{
		$class = get_called_class();
		static::$_disable_filter[$class] = false;
	}

	/**
	 * Enables filtering of deleted entries.
	 */
	public static function enable_filter()
	{
		$class = get_called_class();
		static::$_disable_filter[$class] = true;
	}

	/**
	 * @return boolean True if the deleted items are to be filtered out.
	 */
	public static function get_filter_status()
	{
		$class = get_called_class();
		return \Arr::get(static::$_disable_filter, $class, true);
	}

	/**
	 * Fetches a soft delete property description array, or specific data from it.
	 * Stolen from parent class.
	 *
	 * @param   string  property or property.key
	 * @param   mixed   return value when key not present
	 * @return  mixed
	 */
	public static function soft_delete_property($key, $default = null)
	{
		$class = get_called_class();

		// If already determined
		if ( ! array_key_exists($class, static::$_soft_delete_cached))
		{
			static::soft_delete_properties();
		}

		return \Arr::get(static::$_soft_delete_cached[$class], $key, $default);
	}

	/**
	 * Do some php magic to allow static::find_deleted() to work
	 *
	 * @param type $method
	 * @param type $args
	 */
	public static function __callStatic($method, $args)
	{
		if (strpos($method, 'find_deleted') === 0)
		{
			$temp_args = $args;

			$find_type = count($temp_args) > 0 ? array_pop($temp_args) : 'all';
			$options = count($temp_args) > 0 ? array_pop($temp_args) : array();

			return static::deleted($find_type, $options);
		}

		parent::__callStatic($method, $args);
	}

	/**
	 * Updates the defined deleted_field with a current timestamp rather than
	 * deleting.
	 *
	 * @return this
	 */
	public function delete($cascade = null, $use_transaction = false)
	{
		$deleted_column = static::soft_delete_property('deleted_field', static::$_default_field_name);
		$mysql_timestamp = static::soft_delete_property('mysql_timestamp', static::$_default_mysql_timestamp);

		//If we are using a transcation then make sure it's started
		if ($use_transaction)
		{
			$db = \Database_Connection::instance(static::connection(true));
			$db->start_transaction();
		}

		//Call the observers
		$this->observe('before_delete');

		//Generate the correct timestamp and save it
		$this->{$deleted_column} = $mysql_timestamp ? \Date::forge()->format('mysql') : \Date::forge()->get_timestamp();

		//Loop through all relations and delete if we are cascading.
		$this->freeze();
		foreach ($this->relations() as $rel_name => $rel)
		{
			//get the cascade delete status
			$relCascade = is_null($cascade) ? $rel->cascade_delete : (bool) $cascade;

			//Make sure that the other model is soft delete too
			if ($relCascade)
			{
				if ( ! is_subclass_of($rel->model_to, 'Orm\Model_Soft'))
				{
					//Throw if other is not soft
					throw new RelationNotSoft('Both sides of the relation must be subclasses of Model_Soft if cascade delete is true');
				}

				if(get_class($rel) != 'Orm\ManyMany')
				{
					//Loop through and call delete on all the models
					foreach($rel->get($this) as $model)
					{
						$model->delete($cascade);
					}
				}
			}
		}
		$this->unfreeze();

		$this->save();

		$this->observe('after_delete');

		//Make sure the transaction is commited if needed
		$use_transaction and $db->commit_transaction();

		return $this;
	}

	/**
	 * Allows a soft deleted entry to be restored.
	 */
	public function restore($cascade_restore = null)
	{
		$deleted_column = static::soft_delete_property('deleted_field', static::$_default_field_name);
		$this->{$deleted_column} = null;

		//Loop through all relations and delete if we are cascading.
		$this->freeze();
		foreach ($this->relations() as $rel_name => $rel)
		{
			//get the cascade delete status
			$rel_cascade = is_null($cascade_restore) ? $rel->cascade_delete : (bool) $cascade_restore;

			//Make sure that the other model is soft delete too
			if ($rel_cascade)
			{
				if ( ! is_subclass_of($rel->model_to, 'Orm\Model_Soft'))
				{
					//Throw if other is not soft
					throw new RelationNotSoft('Both sides of the relation must be subclasses of Model_Soft if cascade delete is true');
				}

				if (get_class($rel) != 'Orm\ManyMany')
				{
					$model_to = $rel->model_to;
					$model_to::disable_filter();

					//Loop through and call restore on all the models
					foreach($rel->get($this) as $model)
					{
						$model->restore($cascade_restore);
					}

					$model_to::enable_filter();
				}
			}
		}
		$this->unfreeze();

		$this->save();

		return $this;
	}

	/**
	 * Alias of restore()
	 */
	public function undelete()
	{
		return $this->restore();
	}

	/**
	 * Overrides the find method to allow soft deleted items to be filtered out.
	 */
	public static function find($id = null, array $options = array())
	{
		if (static::get_filter_status())
		{
			//Make sure we are filtering out soft deleted items
			$deleted_column = static::soft_delete_property('deleted_field', static::$_default_field_name);
			$options['where'][] = array($deleted_column, null);
		}

		return parent::find($id, $options);
	}

	/**
	 * Overrides the query method to allow soft delete items to be filtered out.
	 */
	public static function query($options=array())
	{
		if (static::get_filter_status())
		{
			//Make sure we are filtering out soft deleted items
			$deleted_column = static::soft_delete_property('deleted_field', static::$_default_field_name);
			$options['where'][] = array($deleted_column, null);
		}

		return parent::query($options);
	}

	/**
	 * Alisas of find() but selects only deleted entries rather than non-deleted
	 * ones.
	 */
	public static function deleted($id = null, array $options = array())
	{
		//Make sure we are not filtering out soft deleted items
		$deleted_column = static::soft_delete_property('deleted_field', static::$_default_field_name);
		$options['where'][] = array($deleted_column, 'IS NOT', null);

		static::disable_filter();
		$result = parent::find($id, $options);
		static::enable_filter();

		return $result;
	}

}
