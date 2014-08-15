<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\integration\net\http;

use lithium\net\http\Media;
use lithium\net\http\Response;

class MediaTest extends \lithium\test\Integration {

	/**
	 * This tests that setting custom paths and disabling layout
	 * via `\lithium\net\http\Media::type()` is handled properly
	 * by the default `\lithium\template\View` class and `File`
	 * rendered adapter.
	 */
	public function testMediaTypeViewRender() {
		Media::type('view-integration-test', 'lithium/viewtest', array(
			'view' => 'lithium\template\View',
			'paths' => array(
				'layout' => false,
				'template' => array(
					'{:library}/tests/mocks/template/view/adapters/{:template}.{:type}.php',
					'{:library}/tests/mocks/template/view/adapters/{:template}.html.php'
				)
			)
		));

		// testing no layout with a custom type template
		$response = new Response();
		$response->type('view-integration-test');
		Media::render($response, array(), array(
			'layout' => true,
			'library' => 'lithium',
			'template' => 'testTypeFile'
		));
		$this->assertEqual('This is a type test.', $response->body());

		// testing the template falls back to the html template
		$response = new Response();
		$response->type('view-integration-test');
		Media::render($response, array(), array(
			'layout' => true,
			'library' => 'lithium',
			'template' => 'testFile'
		));
		$this->assertEqual('This is a test.', $response->body());

		// testing with a layout
		Media::type('view-integration-test', 'lithium/viewtest', array(
			'view' => 'lithium\template\View',
			'paths' => array(
				'layout' => '{:library}/tests/mocks/template/view/adapters/testLayoutFile.html.php',
				'template' => array(
					'{:library}/tests/mocks/template/view/adapters/{:template}.{:type}.php',
					'{:library}/tests/mocks/template/view/adapters/{:template}.html.php'
				)
			)
		));
		$response = new Response();
		$response->type('view-integration-test');
		Media::render($response, array(), array(
			'layout' => true,
			'library' => 'lithium',
			'template' => 'testTypeFile'
		));
		$this->assertEqual("Layout top.\nThis is a type test.Layout bottom.", $response->body());
	}
}

?>