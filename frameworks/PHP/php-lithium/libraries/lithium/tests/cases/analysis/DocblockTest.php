<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\cases\analysis;

use lithium\analysis\Docblock;
use lithium\analysis\Inspector;

class DocblockTest extends \lithium\test\Unit {

	public function testComment() {
		$expected = array(
			'description' => '',
			'text' => null,
			'tags' => array()
		);
		$result = Docblock::comment('');
		$this->assertEqual($expected, $result);

		$comment = "/**\n * Lithium is cool\n * @foo bar\n * @baz qux\n */";
		$expected = array('description' => 'Lithium is cool', 'text' => '', 'tags' => array());
		$result = Docblock::comment($comment);
		$this->assertEqual($expected, $result);

		Docblock::$tags[] = 'foo';
		Docblock::$tags[] = 'baz';
		$expected['tags'] = array('foo' => 'bar', 'baz' => 'qux');
		$result = Docblock::comment($comment);
		$this->assertEqual($expected, $result);

		$comment = "/**\n * Lithium is cool\n *\n * Very cool\n * @foo bar\n * @baz qux\n */";
		$expected = array(
			'description' => 'Lithium is cool',
			'text' => 'Very cool',
			'tags' => array('foo' => 'bar', 'baz' => 'qux')
		);
		$result = Docblock::comment($comment);
		$this->assertEqual($expected, $result);
	}

	public function testParamTag() {
		$comment = "/**\n * Lithium is cool\n * @param string \$str Some string\n */";
		$expected = array(
			'description' => 'Lithium is cool',
			'text' => '',
			'tags' => array('params' => array(
				'$str' => array('type' => 'string', 'text' => 'Some string')
			))
		);
		$result = Docblock::comment($comment);
		$this->assertEqual($expected, $result);
	}

	/**
	 * This is a short description.
	 *
	 * This is a longer description...
	 * That contains
	 * multiple lines
	 *
	 * @important This is a tag that spans a single line.
	 * @discuss This is a tag that
	 *      spans
	 * several
	 * lines.
	 * @discuss The second discussion item
	 * @link http://example.com/
	 * @see lithium\analysis\Docblock
	 * @return void This tag contains a email@address.com.
	 */
	public function testTagParsing() {
		$info = Inspector::info(__METHOD__ . '()');
		$result = Docblock::comment($info['comment']);
		$this->assertEqual('This is a short description.', $result['description']);

		$expected = "This is a longer description...\nThat contains\nmultiple lines";
		$this->assertEqual($expected, $result['text']);

		$tags = $result['tags'];
		$expected = array('important', 'discuss', 'link', 'see', 'return');
		$this->assertEqual($expected, array_keys($tags));

		$this->assertEqual("This is a tag that\n     spans\nseveral\nlines.", $tags['discuss'][0]);
		$this->assertEqual("The second discussion item", $tags['discuss'][1]);

		$this->assertEqual('void This tag contains a email@address.com.', $tags['return']);
		$this->assertEqual(array(), Docblock::tags(null));

		$this->assertEqual(array('params' => array()), Docblock::tags("Foobar\n\n@param string"));
	}

	public function testDocblockNewlineHandling() {
		$doc  = " * This line as well as the line below it,\r\n";
		$doc .= " * are part of the description.\r\n *\r\n * This line isn't.";
		$result = Docblock::comment($doc);

		$description = "This line as well as the line below it,\nare part of the description.";
		$this->assertEqual($description, $result['description']);

		$this->assertEqual('This line isn\'t.', $result['text']);
	}

	/**
	 * This docblock has an extra * in the closing element.
	 *
	 */
	public function testBadlyClosedDocblock() {
		$info = Inspector::info(__METHOD__ . '()');
		$description = 'This docblock has an extra * in the closing element.';
		$this->assertEqual($description, $info['description']);
		$this->assertEqual('', $info['text']);
	}
}

?>