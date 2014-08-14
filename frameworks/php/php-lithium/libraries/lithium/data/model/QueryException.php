<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\data\model;

/**
 * The `QueryException` is thrown when a CRUD operation on the database returns an
 * error.
 *
 * @see lithium\data\model\Query
 */
class QueryException extends \RuntimeException {

	protected $code = 500;
}

?>