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

class BelongsTo extends Relation
{
	protected $singular = true;

	protected $key_from = array();

	protected $key_to = array('id');

	public function __construct($from, $name, array $config)
	{
		$this->name        = $name;
		$this->model_from  = $from;
		$this->model_to    = array_key_exists('model_to', $config)
			? $config['model_to'] : \Inflector::get_namespace($from).'Model_'.\Inflector::classify($name);
		$this->key_from    = array_key_exists('key_from', $config)
			? (array) $config['key_from'] : (array) \Inflector::foreign_key($this->model_to);
		$this->key_to      = array_key_exists('key_to', $config)
			? (array) $config['key_to'] : $this->key_to;
		$this->conditions  = array_key_exists('conditions', $config)
			? (array) $config['conditions'] : array();

		$this->cascade_save    = array_key_exists('cascade_save', $config)
			? $config['cascade_save'] : $this->cascade_save;
		$this->cascade_delete  = array_key_exists('cascade_delete', $config)
			? $config['cascade_delete'] : $this->cascade_delete;

		if ( ! class_exists($this->model_to))
		{
			throw new \FuelException('Related model not found by Belongs_To relation "'.$this->name.'": '.$this->model_to);
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
				return null;
			}
			$query->where(current($this->key_to), $from->{$key});
			next($this->key_to);
		}

		foreach (\Arr::get($this->conditions, 'where', array()) as $key => $condition)
		{
			is_array($condition) or $condition = array($key, '=', $condition);
			$query->where($condition);
		}
		return $query->get_one();
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

	public function save($model_from, $model_to, $original_model_id, $parent_saved, $cascade)
	{
		if ($parent_saved)
		{
			return;
		}

		if ( ! $model_to instanceof $this->model_to and $model_to !== null)
		{
			throw new \FuelException('Invalid Model instance added to relations in this model.');
		}

		// Save if it's a yet unsaved object
		if ($model_to and $model_to->is_new())
		{
			$model_to->save(false);
		}

		$current_model_id = $model_to ? $model_to->implode_pk($model_to) : null;
		// Check if there was another model assigned (this supersedes any change to the foreign key(s))
		if ($current_model_id != $original_model_id)
		{
			// change the foreign keys in the model_from to point to the new relation
			reset($this->key_from);
			$model_from->unfreeze();
			foreach ($this->key_to as $pk)
			{
				$model_from->{current($this->key_from)} = $model_to ? $model_to->{$pk} : null;
				next($this->key_from);
			}
			$model_from->freeze();
		}
		// if not check the model_from's foreign_keys
		else
		{
			$foreign_keys = count($this->key_to) == 1 ? array($original_model_id) : explode('][', substr($original_model_id, 1, -1));
			$changed      = false;
			$new_rel_id   = array();
			reset($foreign_keys);
			foreach ($this->key_from as $fk)
			{
				if (is_null($model_from->{$fk}))
				{
					$changed = true;
					$new_rel_id = null;
					break;
				}
				elseif ($model_from->{$fk} != current($foreign_keys))
				{
					$changed = true;
				}
				$new_rel_id[] = $model_from->{$fk};
				next($foreign_keys);
			}

			// if any of the keys changed, reload the relationship - saving the object will save those keys
			if ($changed)
			{
				// Attempt to load the new related object
				if ( ! is_null($new_rel_id))
				{
					$rel_obj = call_user_func(array($this->model_to, 'find'), $new_rel_id);
					if (empty($rel_obj))
					{
						throw new \FuelException('New relation set on '.$this->model_from.' object wasn\'t found.');
					}
				}
				else
				{
					$rel_obj = null;
				}

				// Add the new relation to the model_from
				$model_from->unfreeze();
				$rel = $model_from->_relate();
				$rel[$this->name] = $rel_obj;
				$model_from->_relate($rel);
				$model_from->freeze();
			}
		}

		$cascade = is_null($cascade) ? $this->cascade_save : (bool) $cascade;
		if ($cascade and ! empty($model_to))
		{
			$model_to->save();
		}
	}

	public function delete($model_from, $model_to, $parent_deleted, $cascade)
	{
		if ($parent_deleted)
		{
			return;
		}

		// break current relations
		$model_from->unfreeze();
		$rels = $model_from->_relate();
		$rels[$this->name] = null;
		$model_from->_relate($rels);
		foreach ($this->key_from as $fk)
		{
			$model_from->{$fk} = null;
		}
		$model_from->freeze();

		$cascade = is_null($cascade) ? $this->cascade_delete : (bool) $cascade;
		if ($cascade and ! empty($model_to))
		{
			$model_to->delete();
		}
	}
}
