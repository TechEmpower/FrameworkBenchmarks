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

class ManyMany extends Relation
{
	protected $key_from = array('id');

	protected $key_to = array('id');

	/**
	 * @var  string  classname of model to use as connection
	 */
	protected $model_through;

	/**
	 * @var  string  table name of table to use as connection, alternative to $model_through setting
	 */
	protected $table_through;

	/**
	 * @var  string  foreign key of from model in connection table
	 */
	protected $key_through_from;

	/**
	 * @var  string  foreign key of to model in connection table
	 */
	protected $key_through_to;

	public function __construct($from, $name, array $config)
	{
		$this->name        = $name;
		$this->model_from  = $from;
		$this->model_to    = array_key_exists('model_to', $config)
			? $config['model_to'] : \Inflector::get_namespace($from).'Model_'.\Inflector::classify($name);
		$this->key_from    = array_key_exists('key_from', $config)
			? (array) $config['key_from'] : $this->key_from;
		$this->key_to      = array_key_exists('key_to', $config)
			? (array) $config['key_to'] : $this->key_to;
		$this->conditions  = array_key_exists('conditions', $config)
			? (array) $config['conditions'] : array();

		if ( ! empty($config['table_through']))
		{
			$this->table_through = $config['table_through'];
		}
		else
		{
			$table_name = array($this->model_from, $this->model_to);
			natcasesort($table_name);
			$table_name = array_merge($table_name);
			$this->table_through = \Inflector::tableize($table_name[0]).'_'.\Inflector::tableize($table_name[1]);
		}
		$this->key_through_from = ! empty($config['key_through_from'])
			? (array) $config['key_through_from'] : (array) \Inflector::foreign_key($this->model_from);
		$this->key_through_to = ! empty($config['key_through_to'])
			? (array) $config['key_through_to'] : (array) \Inflector::foreign_key($this->model_to);

		$this->cascade_save    = array_key_exists('cascade_save', $config)
			? $config['cascade_save'] : $this->cascade_save;
		$this->cascade_delete  = array_key_exists('cascade_delete', $config)
			? $config['cascade_delete'] : $this->cascade_delete;

		if ( ! class_exists($this->model_to))
		{
			throw new \FuelException('Related model not found by Many_Many relation "'.$this->name.'": '.$this->model_to);
		}
		$this->model_to = get_real_class($this->model_to);
	}

	public function get(Model $from)
	{
		// Create the query on the model_through
		$query = call_user_func(array($this->model_to, 'query'));

		// set the model_from's keys as where conditions for the model_through
		$join = array(
				'table'      => array($this->table_through, 't0_through'),
				'join_type'  => null,
				'join_on'    => array(),
				'columns'    => $this->select_through('t0_through')
		);

		reset($this->key_from);
		foreach ($this->key_through_from as $key)
		{
			if ($from->{current($this->key_from)} === null)
			{
				return array();
			}
			$query->where('t0_through.'.$key, $from->{current($this->key_from)});
			next($this->key_from);
		}

		reset($this->key_to);
		foreach ($this->key_through_to as $key)
		{
			$join['join_on'][] = array('t0_through.'.$key, '=', 't0.'.current($this->key_to));
			next($this->key_to);
		}

		foreach (\Arr::get($this->conditions, 'where', array()) as $key => $condition)
		{
			is_array($condition) or $condition = array($key, '=', $condition);
			$query->where($condition);
		}

		foreach (\Arr::get($this->conditions, 'order_by', array()) as $field => $direction)
		{
			if (is_numeric($field))
			{
				$query->order_by($direction);
			}
			else
			{
				$query->order_by($field, $direction);
			}
		}

		$query->_join($join);

		return $query->get();
	}

	public function select_through($table)
	{
		foreach ($this->key_through_to as $to)
		{
			$properties[] = $table.'.'.$to;
		}
		foreach ($this->key_through_from as $from)
		{
			$properties[] = $table.'.'.$from;
		}

		return $properties;
	}

	public function join($alias_from, $rel_name, $alias_to_nr, $conditions = array())
	{
		$alias_to = 't'.$alias_to_nr;

		$models = array(
			$rel_name.'_through' => array(
				'model'        => null,
				'connection'   => call_user_func(array($this->model_to, 'connection')),
				'table'        => array($this->table_through, $alias_to.'_through'),
				'primary_key'  => null,
				'join_type'    => \Arr::get($conditions, 'join_type') ?: \Arr::get($this->conditions, 'join_type', 'left'),
				'join_on'      => array(),
				'columns'      => $this->select_through($alias_to.'_through'),
				'rel_name'     => $this->model_through,
				'relation'     => $this
			),
			$rel_name => array(
				'model'        => $this->model_to,
				'connection'   => call_user_func(array($this->model_to, 'connection')),
				'table'        => array(call_user_func(array($this->model_to, 'table')), $alias_to),
				'primary_key'  => call_user_func(array($this->model_to, 'primary_key')),
				'join_type'    => \Arr::get($conditions, 'join_type') ?: \Arr::get($this->conditions, 'join_type', 'left'),
				'join_on'      => array(),
				'columns'      => $this->select($alias_to),
				'rel_name'     => strpos($rel_name, '.') ? substr($rel_name, strrpos($rel_name, '.') + 1) : $rel_name,
				'relation'     => $this,
				'where'        => \Arr::get($conditions, 'where', array()),
				'order_by'     => \Arr::get($conditions, 'order_by') ?: \Arr::get($this->conditions, 'order_by', array()),
			)
		);

		reset($this->key_from);
		foreach ($this->key_through_from as $key)
		{
			$models[$rel_name.'_through']['join_on'][] = array($alias_from.'.'.current($this->key_from), '=', $alias_to.'_through.'.$key);
			next($this->key_from);
		}

		reset($this->key_to);
		foreach ($this->key_through_to as $key)
		{
			$models[$rel_name]['join_on'][] = array($alias_to.'_through.'.$key, '=', $alias_to.'.'.current($this->key_to));
			next($this->key_to);
		}
		foreach (\Arr::get($this->conditions, 'where', array()) as $key => $condition)
		{
			! is_array($condition) and $condition = array($key, '=', $condition);
			if ( ! $condition[0] instanceof \Fuel\Core\Database_Expression and strpos($condition[0], '.') === false)
			{
				$condition[0] = $alias_to.'.'.$condition[0];
			}
			is_string($condition[2]) and $condition[2] = \Db::quote($condition[2], $models[$rel_name]['connection']);

			$models[$rel_name]['join_on'][] = $condition;
		}

		return $models;
	}

	public function save($model_from, $models_to, $original_model_ids, $parent_saved, $cascade)
	{
		if ( ! $parent_saved)
		{
			return;
		}

		if ( ! is_array($models_to) and ($models_to = is_null($models_to) ? array() : $models_to) !== array())
		{
			throw new \FuelException('Assigned relationships must be an array or null, given relationship value for '.
				$this->name.' is invalid.');
		}
		$original_model_ids === null and $original_model_ids = array();
		$del_rels = $original_model_ids;

		foreach ($models_to as $key => $model_to)
		{
			if ( ! $model_to instanceof $this->model_to)
			{
				throw new \FuelException('Invalid Model instance added to relations in this model.');
			}

			// Save if it's a yet unsaved object
			if ($model_to->is_new())
			{
				$model_to->save(false);
			}

			$current_model_id = $model_to ? $model_to->implode_pk($model_to) : null;

			// Check if the model was already assigned, if not INSERT relationships:
			if ( ! in_array($current_model_id, $original_model_ids))
			{
				$ids = array();
				reset($this->key_from);
				foreach ($this->key_through_from as $pk)
				{
					$ids[$pk] = $model_from->{current($this->key_from)};
					next($this->key_from);
				}

				reset($this->key_to);
				foreach ($this->key_through_to as $pk)
				{
					$ids[$pk] = $model_to->{current($this->key_to)};
					next($this->key_to);
				}

				\DB::insert($this->table_through)->set($ids)->execute(call_user_func(array($model_from, 'connection')));
				$original_model_ids[] = $current_model_id; // prevents inserting it a second time
			}
			else
			{
				// unset current model from from array of new relations
				unset($del_rels[array_search($current_model_id, $original_model_ids)]);
			}

			// ensure correct pk assignment
			if ($key != $current_model_id)
			{
				$model_from->unfreeze();
				$rel = $model_from->_relate();
				if ( ! empty($rel[$this->name][$key]) and $rel[$this->name][$key] === $model_to)
				{
					unset($rel[$this->name][$key]);
				}
				$rel[$this->name][$current_model_id] = $model_to;
				$model_from->_relate($rel);
				$model_from->freeze();
			}
		}

		// If any ids are left in $del_rels they are no longer assigned, DELETE the relationships:
		foreach ($del_rels as $original_model_id)
		{
			$query = \DB::delete($this->table_through);

			reset($this->key_from);
			foreach ($this->key_through_from as $key)
			{
				$query->where($key, '=', $model_from->{current($this->key_from)});
				next($this->key_from);
			}

			$to_keys = count($this->key_to) == 1 ? array($original_model_id) : explode('][', substr($original_model_id, 1, -1));
			reset($to_keys);
			foreach ($this->key_through_to as $key)
			{
				$query->where($key, '=', current($to_keys));
				next($to_keys);
			}

			$query->execute(call_user_func(array($model_from, 'connection')));
		}

		$cascade = is_null($cascade) ? $this->cascade_save : (bool) $cascade;
		if ($cascade and ! empty($models_to))
		{
			foreach ($models_to as $m)
			{
				$m->save();
			}
		}
	}

	public function delete($model_from, $models_to, $parent_deleted, $cascade)
	{
		if ( ! $parent_deleted)
		{
			return;
		}

		// Remove relations
		$model_from->unfreeze();
		$rels = $model_from->_relate();
		$rels[$this->name] = array();
		$model_from->_relate($rels);
		$model_from->freeze();

		// Delete all relationship entries for the model_from
		$this->delete_related($model_from);

		$cascade = is_null($cascade) ? $this->cascade_delete : (bool) $cascade;
		if ($cascade and ! empty($model_to))
		{
			foreach ($models_to as $m)
			{
				$m->delete();
			}
		}
	}

	public function delete_related($model_from)
	{
		// Delete all relationship entries for the model_from
		$query = \DB::delete($this->table_through);
		reset($this->key_from);
		foreach ($this->key_through_from as $key)
		{
			$query->where($key, '=', $model_from->{current($this->key_from)});
			next($this->key_from);
		}
		$query->execute(call_user_func(array($model_from, 'connection')));
	}
}
