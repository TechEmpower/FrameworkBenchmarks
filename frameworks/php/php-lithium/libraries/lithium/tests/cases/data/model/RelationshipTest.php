<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\cases\data\model;

use lithium\data\model\Relationship;

class RelationshipTest extends \lithium\test\Unit {

	public function testRespondsTo() {
		$query = new Relationship();
		$this->assertTrue($query->respondsTo('foobarbaz'));
		$this->assertFalse($query->respondsTo(0));
	}

}

?>