<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\data\entity;

/**
 * `Record` class. Represents data such as a row from a database. Records have fields (often known
 * as columns in databases).
 */
class Record extends \lithium\data\Entity {

	/**
	 * Converts a `Record` object to another specified format.
	 *
	 * @param string $format The format used by default is `array`
	 * @param array $options
	 * @return mixed
	 */
	public function to($format, array $options = array()) {
		$defaults = array('handlers' => array(
			'stdClass' => function($item) { return $item; }
		));
		$options += $defaults;
		return parent::to($format, $options);
	}
}

?>