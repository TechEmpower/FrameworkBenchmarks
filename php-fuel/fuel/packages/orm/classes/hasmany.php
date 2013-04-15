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

class HasMany extends Relation
{
	public function __construct($from, $name, array $config)
	{
		$this->name        = $name;
		$this->model_from  = $from;
		$this->model_to    = array_key_exists('model_to', $config)
			? $config['model_to'] : \Inflector::get_namespace($from).'Model_'.\Inflector::classify($name);
		$this->key_from    = array_key_exists('key_from', $config)
			? (array) $config['key_from'] : $this->key_from;
		$this->key_to      = array_key_exists('key_to', $config)
			? (array) $config['key_to'] : (array) \Inflector::foreign_key($this->model_from);
		$this->conditions  = array_key_exists('conditions', $config)
			? (array) $config['conditions'] : array();

		$this->cascade_save    = array_key_exists('cascade_save', $config)
			? $config['cascade_save'] : $this->cascade_save;
		$this->cascade_delete  = array_key_exists('cascade_delete', $config)
			? $config['cascade_delete'] : $this->cascade_delete;

		if ( ! class_exists($this->model_to))
		{
			throw new \FuelException('Related model not found by Has_Many relation "'.$this->name.'": '.$this->model_to);
		}
		$this->model_to = get_real_class($this->model_to);
	}

	public function get(Model $from)
	{
		$query = call_user_func(array($this->model_to, 'query'));
		reset($this->key_to);
		foreach ($this->key_from as $key)
		{
			// no point running a query when a key value is null
			if ($from->{$key} === null)
			{
				return array();
			}
			$query->where(current($this->key_to), $from->{$key});
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

		return $query->get();
	}

	public function join($alias_from, $rel_name, $alias_to_nr, $conditions = array())
	{
		$alias_to = 't'.$alias_to_nr;
		$model = array(
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
		);

		reset($this->key_to);
		foreach ($this->key_from as $key)
		{
			$model['join_on'][] = array($alias_from.'.'.$key, '=', $alias_to.'.'.current($this->key_to));
			next($this->key_to);
		}
		foreach (\Arr::get($this->conditions, 'where', array()) as $key => $condition)
		{
			! is_array($condition) and $condition = array($key, '=', $condition);
			if ( ! $condition[0] instanceof \Fuel\Core\Database_Expression and strpos($condition[0], '.') === false)
			{
				$condition[0] = $alias_to.'.'.$condition[0];
			}
			is_string($condition[2]) and $condition[2] = \Db::quote($condition[2], $model['connection']);

			$model['join_on'][] = $condition;
		}

		return array($rel_name => $model);
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

		foreach ($models_to as $key => $model_to)
		{
			if ( ! $model_to instanceof $this->model_to)
			{
				throw new \FuelException('Invalid Model instance added to relations in this model.');
			}

			$current_model_id = ($model_to and ! $model_to->is_new()) ? $model_to->implode_pk($model_to) : null;

			// Check if the model was already assigned
			if (($model_to and $model_to->is_new()) or ! in_array($current_model_id, $original_model_ids))
			{
				// assign this object to the new objects foreign keys
				reset($this->key_to);
				$frozen = $model_to->frozen(); // only unfreeze/refreeze when it was frozen
				$frozen and $model_to->unfreeze();
				foreach ($this->key_from as $pk)
				{
					$model_to->{current($this->key_to)} = $model_from->{$pk};
					next($this->key_to);
				}
				$model_to->is_new() and $model_to->save(false);
				$frozen and $model_to->freeze();
			}
			// check if the model_to's foreign_keys match the model_from's primary keys
			else
			{
				// unset current model from from array
				unset($original_model_ids[array_search($current_model_id, $original_model_ids)]);

				// check if model_to still refers to this model_from
				$changed = false;
				reset($this->key_to);
				foreach ($this->key_from as $pk)
				{
					if ($model_to->{current($this->key_to)} != $model_from->{$pk})
					{
						$changed = true;
					}
					next($this->key_to);
				}

				// if any of the keys changed, the relationship was broken - remove model_to from loaded objects
				if ($changed)
				{
					$model_from->unfreeze();
					$rel = $model_from->_relate();
					unset($rel[$this->name][$key]);
					$model_from->_relate($rel);
					$model_from->freeze();

					// cascading this change won't work here, save just the object with cascading switched off
					$model_from->save(false);
				}
			}

			// Fix it if key isn't an imploded PK
			if ($key != ($current_model_id = $model_to->implode_pk($model_to)))
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

		// if any original ids are left over in the array, they're no longer related - break them
		foreach ($original_model_ids as $original_model_id)
		{
			// if still loaded set this object's old relation's foreign keys to null
			if ($original_model_id and $obj = call_user_func(array($this->model_to, 'find'),
				count($this->key_to) == 1 ? array($original_model_id) : explode('][', substr($original_model_id, 1, -1))))
			{
				$frozen = $obj->frozen(); // only unfreeze/refreeze when it was frozen
				$frozen and $obj->unfreeze();
				foreach ($this->key_to as $fk)
				{
					$obj->{$fk} = null;
				}
				$frozen and $obj->freeze();

				// cascading this change won't work here, save just the object with cascading switched off
				$obj->save(false);
			}
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

		// break current relations
		$model_from->unfreeze();
		$rels = $model_from->_relate();
		$rels[$this->name] = array();
		$model_from->_relate($rels);
		$model_from->freeze();

		foreach ($models_to as $model_to)
		{
			if ( ! $model_to->frozen())
			{
				foreach ($this->key_to as $fk)
				{
					$model_to->{$fk} = null;
				}
			}
		}

		if ( ! empty($models_to))
		{
			$cascade = is_null($cascade) ? $this->cascade_delete : (bool) $cascade;
			foreach ($models_to as $m)
			{
				if ($cascade)
				{
					$m->delete();
				}
				else
				{
					$m->is_changed() and $m->save();
				}
			}
		}
	}
}
