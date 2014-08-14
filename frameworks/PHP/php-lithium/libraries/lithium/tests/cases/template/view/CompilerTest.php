<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\cases\template\view;

use lithium\core\Libraries;
use lithium\template\view\Compiler;

class CompilerTest extends \lithium\test\Unit {

	protected $_path;

	protected $_file = 'template.html.php';

	public function skip() {
		$path = realpath(Libraries::get(true, 'resources') . '/tmp/tests');
		$this->skipIf(!is_writable($path), "Path `{$path}` is not writable.");

		$path = realpath(Libraries::get(true, 'resources') . '/tmp/cache/templates');
		$this->skipIf(!is_writable($path), "Path `{$path}` is not writable.");
	}

	public function setUp() {
		$this->_path = realpath(
			str_replace('\\', '/', Libraries::get(true, 'resources')) . '/tmp/tests'
		);

		file_put_contents("{$this->_path}/{$this->_file}", "
			<?php echo 'this is unescaped content'; ?" . ">
			<?='this is escaped content'; ?" . ">
			<?=\$alsoEscaped; ?" . ">
			<?=\$this->escape('this is also escaped content'); ?" . ">
			<?=\$this->escape(
				'this, too, is escaped content'
			); ?" . ">
			<?='This is
				escaped content
				that breaks over
				several lines
			'; ?" . ">
			<?=\$h('This is pre-escaped content'); ?>
		");
	}

	public function tearDown() {
		$path = realpath(Libraries::get(true, 'resources') . '/tmp/cache/templates');

		foreach (glob("{$path}/*.php") as $file) {
			unlink($file);
		}
		unlink("{$this->_path}/{$this->_file}");
	}

	public function testTemplateContentRewriting() {
		$template = Compiler::template("{$this->_path}/{$this->_file}");
		$this->assertTrue(file_exists($template));

		$expected = array(
			"<?php echo 'this is unescaped content'; ?" . ">",
			"<?php echo \$h('this is escaped content'); ?" . ">",
			"<?php echo \$h(\$alsoEscaped); ?" . ">",
			"<?php echo \$this->escape('this is also escaped content'); ?" . ">",
			'<?php echo $this->escape(',
			"'this, too, is escaped content'",
			'); ?>',
			"<?php echo \$h('This is",
			'escaped content',
			'that breaks over',
			'several lines',
			"'); ?>",
			"<?php echo \$h('This is pre-escaped content'); ?>"
		);
		$result = array_map('trim', explode("\n", trim(file_get_contents($template))));
		$this->assertEqual($expected, $result);
	}

	public function testFallbackWithNonWritableDirectory() {
		$this->expectException('/failed to open stream/');
		$result = Compiler::template("{$this->_path}/{$this->_file}", array(
			'path' => LITHIUM_APP_PATH . '/foo',
			'fallback' => true
		));
		$this->assertEqual("{$this->_path}/{$this->_file}", $result);

		$this->expectException('/Could not write compiled template/');
		$this->expectException('/failed to open stream/');
		$result = Compiler::template("{$this->_path}/{$this->_file}", array(
			'path' => LITHIUM_APP_PATH . '/foo',
			'fallback' => false
		));
	}

	public function testTemplateCacheHit() {
		$path = Libraries::get(true, 'resources') . '/tmp/cache/templates';
		$original = Compiler::template("{$this->_path}/{$this->_file}", compact('path'));
		$cache = glob("{$path}/*");
		clearstatcache();

		$cached = Compiler::template("{$this->_path}/{$this->_file}", compact('path'));
		$this->assertEqual($original, $cached);
		$this->assertEqual($cache, glob("{$path}/*"));

		file_put_contents("{$this->_path}/{$this->_file}", "Updated");
		clearstatcache();
		$updated = Compiler::template("{$this->_path}/{$this->_file}", compact('path'));
		$newCache = glob("{$path}/*");

		$this->assertNotEqual($cache, $updated);
		$this->assertEqual(count($cache), count($newCache));
		$this->assertNotEqual($cache, $newCache);
	}
}

?>