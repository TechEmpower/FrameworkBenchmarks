<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

/**
 * Message data for `fr`.
 *
 * Plural rule and forms derived from the GNU gettext documentation.
 *
 * @link http://www.gnu.org/software/gettext/manual/gettext.html#Plural-forms
 */
return array(
	'pluralForms' => 2,
	'pluralRule' => function ($n) { return $n > 1 ? 1 : 0; }
);

?>