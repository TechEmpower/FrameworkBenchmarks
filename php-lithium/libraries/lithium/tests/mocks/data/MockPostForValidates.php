<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\mocks\data;

use lithium\util\Validator;

class MockPostForValidates extends \lithium\data\Model {

	protected $_meta = array('source' => 'mock_posts', 'connection' => false);

	public $validates = array(
		'title' => 'please enter a title',
		'email' => array(
			array('notEmpty', 'message' => 'email is empty'),
			array('email', 'message' => 'email is not valid'),
			array('modelIsSet', 'required' => false, 'message' => 'model is not set'),
			array(
				'inList',
				'list' => array('something@test.com','foo@bar.com'),
				'on' => 'customEvent',
				'message' => 'email is not in 1st list'
			),
			array(
				'inList',
				'list' => array('something@test.com'),
				'on' => 'anotherCustomEvent',
				'message' => 'email is not in 2nd list'
			)
		)
	);

	public static function __init() {
		$class = __CLASS__;
		Validator::add('modelIsSet', function($value, $format, $options) use ($class) {
			if (isset($options['model']) && $options['model'] = $class) {
				return true;
			}
			return false;
		});
	}
}

?>