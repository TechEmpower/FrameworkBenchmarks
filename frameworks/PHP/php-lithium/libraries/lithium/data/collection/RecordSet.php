<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\data\collection;

use lithium\util\Set;

class RecordSet extends \lithium\data\Collection {

	/**
	 * A 2D array of column-mapping information, where the top-level key is the fully-namespaced
	 * model name, and the sub-arrays are column names.
	 *
	 * @var array
	 */
	protected $_columns = array();

	/**
	 * A recursive array of relation dependencies where key are relations
	 * and value are arrays with their relation dependencies
	 *
	 * @var array
	 */
	protected $_dependencies = array();

	/**
	 * Precompute index of the main model primary key(s) which allow to find
	 * values directly is result data without the column name matching process
	 *
	 * @var array
	 */
	protected $_keyIndex = array();

	/**
	 * Initializes the record set and uses the database connection to get the column list contained
	 * in the query that created this object.
	 *
	 * @see lithium\data\collection\RecordSet::$_columns
	 * @return void
	 * @todo The part that uses _handle->schema() should be rewritten so that the column list
	 *       is coming from the query object.
	 */
	protected function _init() {
		parent::_init();
		if ($this->_result) {
			$this->_columns = $this->_columnMap();
			if ($this->_query) {
				$columns = array_filter(array_keys($this->_columns));
				$this->_dependencies = Set::expand(Set::normalize($columns));
				$this->_keyIndex = $this->_keyIndex('');
			}
		}
	}

	/**
	 * Extract the next item from the result ressource and wraps it into a `Record` object.
	 *
	 * @return mixed Returns the next `Record` if exists. Returns `null` otherwise
	 */
	protected function _populate() {
		if ($this->closed() || !$this->_result->valid()) {
			return;
		}

		$data = $this->_result->current();
		if ($this->_query) {
			$data = $this->_mapRecord($data);
		}
		$result = $this->_set($data, null, array('exists' => true));
		$this->_result->next();

		return $result;
	}

	protected function _set($data = null, $offset = null, $options = array()) {
		if ($model = $this->_model) {
			$data = !is_object($data) ? $model::connection()->item($model, $data, $options) : $data;
			$key = $model::key($data);
		} else {
			$key = $offset;
		}
		if (is_array($key)) {
			$key = count($key) === 1 ? current($key) : null;
		}
		return $key !== null ? $this->_data[$key] = $data : $this->_data[] = $data;
	}

	/**
	 * Convert a PDO `Result` array to a nested `Record` object
	 *
	 * @param array $data 2 dimensional PDO `Result` array
	 * @return object Returns a `Record` object
	 */
	protected function _mapRecord($data) {
		$primary = $this->_model;
		$conn = $primary::connection();
		$main = $record = array();
		$i = 0;

		foreach ($this->_keyIndex as $key => $value) {
			$main[$key] = $data[$key];
		}

		do {
			$offset = 0;
			if ($i != 0) {
				$keys = array();
				foreach ($this->_keyIndex as $key => $value) {
					$keys[$key] = $data[$key];
				}
				if ($main != $keys) {
					$this->_result->prev();
					break;
				}
			}
			foreach ($this->_columns as $name => $fields) {
				$fieldCount = count($fields);
				$record[$i][$name] = array_combine(
					$fields, array_slice($data, $offset, $fieldCount)
				);
				$offset += $fieldCount;
			}
			$i++;
		} while ($main && $data = $this->_result->next());

		$relMap = $this->_query->relationships();
		return $this->_hydrateRecord(
			$this->_dependencies, $primary, $record, 0, $i, '', $relMap, $conn
		);
	}

	/**
	 * Hydrate a 2 dimensional PDO `Result` array
	 *
	 * @param array $relations The cascading with relation
	 * @param string $primary Model classname
	 * @param array $record Loaded Records
	 * @param integer $min
	 * @param integer $max
	 * @param string $name Alias name
	 * @param array $relMap The query relationships array
	 * @param object $conn The connection object
	 * @return object Returns a `Record` object
	 */
	protected function _hydrateRecord($relations, $primary, $record, $min, $max, $name, &$relMap, $conn) {
		$options = array('exists' => true);

		$count = count($record);
		if (!empty($relations)) {
			foreach ($relations as $relation => $subrelations) {
				$relName = $name ? $name . '.' . $relation : $relation;
				$field = $relMap[$relName]['fieldName'];
				$relModel = $relMap[$relName]['model'];

				if ($relMap[$relName]['type'] === 'hasMany') {
					$rel = array();
					$main = $relModel::key($record[$min][$relName]);
					$i = $min;
					$j = $i + 1;
					while ($j < $max) {
						$keys = $relModel::key($record[$j][$relName]);
						if ($main != $keys) {
							$rel[] = $this->_hydrateRecord(
								$subrelations, $relModel, $record, $i, $j, $relName, $relMap, $conn
							);
							$main = $keys;
							$i = $j;
						}
						$j++;
					}
					if (array_filter($record[$i][$relName])) {
						$rel[] = $this->_hydrateRecord(
							$subrelations, $relModel, $record, $i, $j, $relName, $relMap, $conn
						);
					}
					$opts = array('class' => 'set') + $options;
					$record[$min][$name][$field] = $conn->item($primary, $rel, $opts);
				} else {
					$record[$min][$name][$field] = $this->_hydrateRecord(
						$subrelations, $relModel, $record, $min, $max, $relName, $relMap, $conn
					);
				}
			}
		}
		return $conn->item(
			$primary, isset($record[$min][$name]) ? $record[$min][$name] : array(), $options
		);
	}

	protected function _columnMap() {
		if ($this->_query && $map = $this->_query->map()) {
			return $map;
		}
		if (!($model = $this->_model)) {
			return array();
		}
		if (!is_object($this->_query) || !$this->_query->join()) {
			$map = $model::connection()->schema($this->_query);
			return $map;
		}
		$model = $this->_model;
		$map = $model::connection()->schema($this->_query);

		return $map;
	}

	/**
	 * Result object contain SQL result which are generally a 2 dimentionnal array
	 * where line are records and columns are fields.
	 * This function extract from the Result object the index of primary key(s).
	 *
	 * @param string $name The name of the relation to retreive the index of
	 *               corresponding primary key(s).
	 * @return array An array where key are index and value are primary key fieldname
	 */
	protected function _keyIndex($name) {
		if (!($model = $this->_model) || !isset($this->_columns[$name])) {
			return array();
		}
		$index = 0;
		foreach ($this->_columns as $key => $value) {
			if ($key === $name) {
				$flip = array_flip($value);
				$keys = $model::meta('key');
				if (!is_array($keys)) {
					$keys = array($keys);
				}
				$keys = array_flip($keys);
				$keys = array_intersect_key($flip, $keys);
				foreach ($keys as &$value) {
					$value += $index;
				}
				return array_flip($keys);
			}
			$index += count($value);
		}
	}
}

?>