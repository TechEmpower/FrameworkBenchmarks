<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

/**
 * Validation data for `en_US`.
 */
return array(
	'phone' => '/^(?:\+?1)?[-. ]?\\(?[2-9][0-8][0-9]\\)?[-. ]?[2-9][0-9]{2}[-. ]?[0-9]{4}$/',
	'postalCode' => '/\\A\\b[0-9]{5}(?:-[0-9]{4})?\\b\\z/i',
	'ssn' => '/\\A\\b[0-9]{3}-[0-9]{2}-[0-9]{4}\\b\\z/i'
);

?>