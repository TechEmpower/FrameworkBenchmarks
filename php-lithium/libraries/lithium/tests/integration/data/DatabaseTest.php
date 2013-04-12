<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\integration\data;

use lithium\data\Connections;
use lithium\data\model\Query;
use lithium\data\source\Database;
use lithium\tests\mocks\data\source\Images;
use lithium\tests\mocks\data\source\Galleries;
use lithium\util\String;

class DatabaseTest extends \lithium\test\Integration {

	public $db = null;
	protected $_dbConfig;

	public $images = array(
			1 => array(
				'id' => 1,
				'gallery_id' => 1,
				'image' => 'someimage.png',
				'title' => 'Image1 Title'
			),
			2 => array(
				'id' => 2,
				'gallery_id' => 1,
				'image' => 'anotherImage.jpg',
				'title' => 'Our Vacation'
			),
			3 => array(
				'id' => 3,
				'gallery_id' => 1,
				'image' => 'me.bmp',
				'title' => 'Me.'
			),
			4 => array(
				'id' => 4,
				'gallery_id' => 2,
				'image' => 'picture.jpg',
				'title' => 'Obi-Wan Kenobi'
			),
			5 => array(
				'id' => 5,
				'gallery_id' => 2,
				'image' => 'unknown.gif',
				'title' => 'John Doe'
			)
		);

	public $gallery = array('name' => 'Foo Gallery');

	public $galleries = array(
		1 => array('id' => 1, 'name' => 'Foo Gallery'),
		2 => array('id' => 2, 'name' => 'Bar Gallery')
	);

	public function setUp() {
		$mockBase = LITHIUM_LIBRARY_PATH . '/lithium/tests/mocks/data/source/database/adapter/';
		$files = array('galleries' => '_galleries.sql', 'images' => '_images.sql');
		$files = array_diff_key($files, array_flip($this->db->sources()));

		foreach ($files as $file) {
			$sqlFile = $mockBase . strtolower($this->_dbConfig['adapter']) . $file;
			$this->skipIf(!file_exists($sqlFile), "SQL file $sqlFile does not exist.");
			$sql = file_get_contents($sqlFile);
			$this->db->read($sql, array('return' => 'resource'));
		}
	}

	public function tearDown() {
		$this->db->read('DROP TABLE IF EXISTS `images`;');
		$this->db->read('DROP TABLE IF EXISTS `galleries`;');
	}

	public function skip() {
		$connection = 'lithium_mysql_test';
		$this->_dbConfig = Connections::get($connection, array(
			'config' => true
		));
		$isConnected = $this->_dbConfig && Connections::get($connection)->isConnected(array(
			'autoConnect' => true
		));
		$isAvailable = $this->_dbConfig && $isConnected;
		$this->skipIf(!$isAvailable, "No {$connection} connection available.");

		$this->db = Connections::get($connection);
		$this->skipIf(
			!($this->db instanceof Database),
			"The {$connection} connection is not a relational database."
		);
	}

	public function testConnectWithNoDatabase() {
		$config = $this->_dbConfig;
		$config['database'] = null;
		$connection = 'no_database';
		Connections::add($connection, $config);
		$this->expectException("/No Database configured/");
		Connections::get($connection)->connect();
	}

	public function testConnectWithWrongHost() {
		$config = $this->_dbConfig;
		$config['host'] = 'unknown.host.nowhere';
		$connection = 'wrong_host';
		Connections::add($connection, $config);
		$this->expectException('/Unable to connect to host `unknown.host.nowhere`/');
		Connections::get($connection)->connect();
	}

	public function testConnectWithWrongPassword() {
		$config = $this->_dbConfig;
		$config['login'] = 'wrong_login';
		$config['password'] = 'wrong_pass';
		$connection = 'wrong_passord';
		Connections::add($connection, $config);
		$this->expectException('/Host connected, but could not access database/');
		Connections::get($connection)->connect();
	}

	public function testExecuteException() {
		$this->expectException("/You have an error(.*?)near '\* FROM table'/");
		$this->db->read('SELECT * FROM * FROM table');
	}

	public function testCreateData() {
		$gallery = Galleries::create($this->gallery);
		$this->assertTrue($gallery->save());
		$this->assertTrue($gallery->id);

		foreach ($this->images as $key => $image) {
			unset($image['id'], $image['gallery_id']);
			$img = Images::create($image + array('gallery_id' => $gallery->id));
			$this->assertEqual(true, $img->save());
			$this->assertEqual($gallery->id, $img->gallery_id);
		}
	}

	public function testManyToOne() {
		$this->_createGalleriesWithImages();
		$opts = array('conditions' => array('gallery_id' => 1));

		$query = new Query($opts + array(
			'type' => 'read',
			'model' => 'lithium\tests\mocks\data\source\Images',
			'source' => 'images',
			'alias' => 'Images',
			'with' => array('Galleries')
		));
		$images = $this->db->read($query)->data();
		reset($this->images);

		$this->assertEqual(3, count($images));
		foreach ($images as $key => $image) {
			$expect = current($this->images) + array(
				'gallery_id' => 1,
				'gallery' => $this->galleries[1]
			);
			$this->assertEqual($expect, $image);
			next($this->images);
		}

		$images = Images::find('all', $opts + array('with' => 'Galleries'))->data();
		reset($this->images);

		foreach ($images as $key => $image) {
			$expect = (array) current($this->images) + array('gallery' => $this->galleries[1]);
			ksort($expect);
			ksort($image);
			$this->assertEqual($expect, $image);
			next($this->images);
		}
	}

	public function testOneToMany() {
		$this->_createGalleriesWithImages();
		$opts = array('conditions' => array('Galleries.id' => 1));

		$query = new Query($opts + array(
			'type' => 'read',
			'model' => 'lithium\tests\mocks\data\source\Galleries',
			'source' => 'galleries',
			'alias' => 'Galleries',
			'with' => array('Images')
		));
		$galleries = $this->db->read($query)->data();
		$images = array(1 => $this->images[1], 2 => $this->images[2], 3 => $this->images[3]);

		$this->assertEqual(1, count($galleries));
		foreach ($galleries as $key => $gallery) {
			$expect = $this->galleries[1] + array(
				'images' => $images
			);
			$this->assertEqual($expect, $gallery);
		}

		$gallery = Galleries::find('first', $opts + array('with' => 'Images'))->data();
		$expect = $this->galleries[1] + array('images' => $images);
		$this->assertEqual($expect, $gallery);
	}

	public function testUpdate() {
		$this->_createGalleriesWithImages();
		$options = array('conditions' => array('gallery_id' => 1));
		$uuid = String::uuid();
		$image = Images::find('first', $options);
		$image->title = $uuid;
		$firstID = $image->id;
		$image->save();
		$this->assertEqual($uuid, Images::find('first', $options)->title);

		$uuid = String::uuid();
		Images::update(array('title' => $uuid), array('id' => $firstID));
		$this->assertEqual($uuid, Images::find('first', $options)->title);
		$this->images[0]['title'] = $uuid;
	}

	public function testFields() {
		$this->_createGalleriesWithImages();
		$fields = array('id', 'image');
		$image = Images::find('first', array(
			'fields' => $fields,
			'conditions' => array(
				'gallery_id' => 1
			)
		));
		$this->assertEqual($fields, array_keys($image->data()));
	}

	public function testOrder() {
		$this->_createGalleriesWithImages();
		$images = Images::find('all', array(
			'order' => 'id DESC',
			'conditions' => array(
				'gallery_id' => 1
			)
		))->data();

		$expected = array($this->images[3], $this->images[2], $this->images[1]);

		$this->assertEqual(3, count($images));
		foreach ($images as $image) {
			$this->assertEqual(current($expected), $image);
			next($expected);
		}
	}

	public function testGroup() {
		$this->_createGalleriesWithImages();

		$galleries = Galleries::find('all', array(
			'fields' => array(array('count(Images.id) AS count')),
			'with' => 'Images',
			'group' => array('Galleries.id'),
			'order' => array('Galleries.id' => 'ASC')
		));

		$this->assertEqual(2, count($galleries));
		$expected = array(3, 2);

		foreach ($galleries as $gallery) {
			$this->assertEqual(current($expected), $gallery->count);
			next($expected);
		}
	}

	public function testRemove() {
		$this->assertTrue(Galleries::remove());
		$this->assertTrue(Images::remove());
	}

	protected function _createGalleries() {
		foreach ($this->galleries as $key => $gallery) {
			$entity = Galleries::create($gallery);
			$this->assertEqual(true, $entity->save());
		}
	}
	protected function _createGalleriesWithImages() {
		$this->_createGalleries();
		foreach ($this->images as $key => $image) {
			$entity = Images::create($image);
			$this->assertEqual(true, $entity->save());
		}
	}
}

?>