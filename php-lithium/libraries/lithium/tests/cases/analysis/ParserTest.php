<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\cases\analysis;

use lithium\core\Libraries;
use lithium\analysis\Parser;

class ParserTest extends \lithium\test\Unit {

	/**
	 * Tests that PHP code snippets properly resolve to their corresponding tokens.
	 *
	 * @return void
	 */
	public function testSingleTokenization() {
		$result = Parser::token('static');
		$this->assertEqual('T_STATIC', $result);

		$result = Parser::token('=>');
		$this->assertEqual('T_DOUBLE_ARROW', $result);

		$result = Parser::token(' =>');
		$this->assertEqual('T_WHITESPACE', $result);

		$result = Parser::token('static =>');
		$this->assertEqual('T_STATIC', $result);

		$result = Parser::token("\nstatic =>");
		$this->assertEqual('T_WHITESPACE', $result);

		$this->assertFalse(Parser::token(''));

		$result = Parser::token(';');
		$this->assertEqual(';', $result);

		$result = Parser::token('"string"');
		$this->assertEqual('T_CONSTANT_ENCAPSED_STRING', $result);

		$result = Parser::token('1');
		$this->assertEqual('T_LNUMBER', $result);

		$result = Parser::token('0');
		$this->assertEqual('T_LNUMBER', $result);

		$result = Parser::token('0');
		$this->assertEqual('T_LNUMBER', $result);
	}

	public function testFullTokenization() {
		$result = Parser::tokenize('$foo = function() {};');
		$this->assertEqual(11, count($result));

		$expected = array(
			'id' => T_VARIABLE,
			'name' => 'T_VARIABLE',
			'content' => '$foo',
			'line' => 1
		);
		$this->assertEqual($expected, $result[0]);

		$expected = array('id' => null, 'name' => ';', 'content' => ';', 'line' => 1);
		$this->assertEqual($expected, $result[10]);

		$code = '$defaults = array("id" => "foo", "name" => "bar", \'count\' => 5);';
		$result = Parser::tokenize($code);

		$this->assertEqual(27, count($result));
		$this->assertEqual('T_VARIABLE', $result[0]['name']);
		$this->assertEqual('$defaults', $result[0]['content']);
	}

	public function testTokenPatternMatching() {
		$code = '$defaults = array("id" => "foo", "name" => "bar", \'count\' => 5);';

		$result = Parser::match($code, array('"string"'), array('return' => 'content'));
		$expected = array('"id"', '"foo"', '"name"', '"bar"', '\'count\'');
		$this->assertEqual($expected, $result);

		$result = Parser::match(
			$code,
			array('"string"' => array('before' => '=>'), '1' => array('before' => '=>')),
			array('return' => 'content')
		);
		$expected = array('"foo"', '"bar"', '5');
		$this->assertEqual($expected, $result);

		$result = Parser::match($code, array('"string"' => array('after' => '=>')), array(
			'return' => 'content'
		));
		$expected = array ('"id"', '"name"', '\'count\'');
		$this->assertEqual($expected, $result);
	}

	public function testFilteredTokenization() {
		$code = 'while (isset($countRugen)) { if ($inigoMontoya->is("alive")) { ' . "\n";
		$code .= '$inigoMontoya->say(array("hello", "name", "accusation", "die")); ' . "\n";
		$code .= 'try { $inigoMontoya->kill($countRugen); } catch (Exception $e) { continue; } } }';

		$result = Parser::tokenize($code, array('include' => array('T_IF', 'T_WHILE', 'T_CATCH')));
		$expected = array(
			array('id' => T_WHILE, 'name' => 'T_WHILE', 'content' => 'while', 'line' => 1),
			array('id' => T_IF, 'name' => 'T_IF', 'content' => 'if', 'line' => 1),
			array('id' => T_CATCH, 'name' => 'T_CATCH', 'content' => 'catch', 'line' => 3)
		);
		$this->assertEqual($expected, $result);
	}

	public function testFindingTokenPatterns() {
		$code = file_get_contents(Libraries::path('lithium\analysis\Parser'));

		$expected = array('tokenize', 'matchToken', '_prepareMatchParams', 'token');
		$results = array_values(array_unique(array_map(function($i) { return $i[0]; }, Parser::find(
			$code, 'static::_(*)', array('capture' => array('T_STRING'), 'return' => 'content')
		))));

		$this->assertEqual($expected, $results);

		$expected = array('lithium\util\Set', 'lithium\util\Collection');
		$results = array_map(
			function ($i) { return join('', $i); },
			$results = Parser::find($code, 'use *;', array(
				'return'      => 'content',
				'lineBreaks'  => true,
				'startOfLine' => true,
				'capture'     => array('T_STRING', 'T_NS_SEPARATOR')
			))
		);
		$this->assertEqual($expected, $results);

		$code = 'function test($options) { return function($foo) use ($options) {';
		$code .= ' ClassName::method($options); ' . "\n" . ' $foo->method($options); }; }';
		list($results) = Parser::find($code, '_::_(', array(
			'capture' => array('T_STRING'), 'return' => 'content'
		));
		$expected = array('ClassName', 'method');
		$this->assertEqual($expected, $results);
	}

	public function testParserGuessesLineBleed() {
		$code = <<<EOD
if (false) {
	return true;
}
EOD;
		$tokens = Parser::tokenize($code);
		$this->assertIdentical('}', $tokens[13]['content']);
		$this->assertIdentical(3, $tokens[13]['line']);
	}

	public function testParserGuessesLineBleedWithNonWhitespace() {
		$code = <<<EOD
if (false) {
	// hello world
}
EOD;
		$tokens = Parser::tokenize($code);
		$this->assertIdentical('}', $tokens[9]['content']);
		$this->assertIdentical(3, $tokens[9]['line']);
	}

}

?>