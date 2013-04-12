<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\cases\util;

use stdClass;
use lithium\util\Set;

class SetTest extends \lithium\test\Unit {

	public function testDepthWithEmptyData() {
		$data = array();
		$result = Set::depth($data);
		$this->assertEqual(0, $result);
	}

	public function testDepthOneLevelWithDefaults() {
		$data = array();
		$result = Set::depth($data);
		$this->assertEqual(0, $result);

		$data = array('one', '2', 'three');
		$result = Set::depth($data);
		$this->assertEqual(1, $result);

		$data = array('1' => '1.1', '2', '3');
		$result = Set::depth($data);
		$this->assertEqual(1, $result);

		$data = array('1' => '1.1', '2', '3' => array('3.1' => '3.1.1'));
		$result = Set::depth($data, array('all' => false));
		$this->assertEqual(1, $result);
	}

	public function testDepthTwoLevelsWithDefaults() {
		$data = array('1' => array('1.1' => '1.1.1'), '2', '3' => array('3.1' => '3.1.1'));
		$result = Set::depth($data);
		$this->assertEqual(2, $result);

		$data = array('1' => array('1.1' => '1.1.1'), '2', '3' => array('3.1' => array(
			'3.1.1' => '3.1.1.1'
		)));
		$result = Set::depth($data);
		$this->assertEqual($result, 2);

		$data = array(
			'1' => array('1.1' => '1.1.1'),
			array('2' => array(
				'2.1' => array('2.1.1' => array('2.1.1.1' => '2.1.1.1.1'))
			)),
			'3' => array('3.1' => array('3.1.1' => '3.1.1.1'))
		);
		$result = Set::depth($data, array('all' => false));
		$this->assertEqual($result, 2);
	}

	public function testDepthTwoLevelsWithAll() {
		$data = array('1' => '1.1', '2', '3' => array('3.1' => '3.1.1'));
		$result = Set::depth($data, array('all' => true));
		$this->assertEqual(2, $result);
	}


	public function testDepthThreeLevelsWithAll() {
		$data = array(
			'1' => array('1.1' => '1.1.1'), '2', '3' => array('3.1' => array('3.1.1' => '3.1.1.1'))
		);
		$result = Set::depth($data, array('all' => true));
		$this->assertEqual(3, $result);

		$data = array(
			'1' => array('1.1' => '1.1.1'),
			array('2' => array('2.1' => array('2.1.1' => '2.1.1.1'))),
			'3' => array('3.1' => array('3.1.1' => '3.1.1.1'))
		);
		$result = Set::depth($data, array('all' => true));
		$this->assertEqual(4, $result);

		$data = array(
			'1' => array('1.1' => '1.1.1'),
			array('2' => array('2.1' => array('2.1.1' => array('2.1.1.1')))),
			'3' => array('3.1' => array('3.1.1' => '3.1.1.1'))
		);
		$result = Set::depth($data, array('all' => true));
		$this->assertEqual(5, $result);

		$data = array(
			'1' => array('1.1' => '1.1.1'), array(
			'2' => array('2.1' => array('2.1.1' => array('2.1.1.1' => '2.1.1.1.1')))),
			'3' => array('3.1' => array('3.1.1' => '3.1.1.1'))
		);
		$result = Set::depth($data, array('all' => true));
		$this->assertEqual(5, $result);
	}

	public function testDepthFourLevelsWithAll() {
		$data = array(
			'1' => array('1.1' => '1.1.1'), array(
			'2' => array('2.1' => array('2.1.1' => '2.1.1.1'))),
			'3' => array('3.1' => array('3.1.1' => '3.1.1.1'))
		);
		$result = Set::depth($data, array('all' => true));
		$this->assertEqual(4, $result);
	}

	public function testDepthFiveLevelsWithAll() {
		$data = array(
			'1' => array('1.1' => '1.1.1'), array(
			'2' => array('2.1' => array('2.1.1' => array('2.1.1.1')))),
			'3' => array('3.1' => array('3.1.1' => '3.1.1.1'))
		);
		$result = Set::depth($data, array('all' => true));
		$this->assertEqual(5, $result);

		$data = array('1' => array('1.1' => '1.1.1'), array(
			'2' => array('2.1' => array('2.1.1' => array('2.1.1.1' => '2.1.1.1.1')))),
			'3' => array('3.1' => array('3.1.1' => '3.1.1.1'))
		);
		$result = Set::depth($data, array('all' => true));
		$this->assertEqual(5, $result);
	}

	public function testFlattenOneLevel() {
		$data = array('Larry', 'Curly', 'Moe');
		$result = Set::flatten($data);
		$this->assertEqual($result, $data);

		$data[9] = 'Shemp';
		$result = Set::flatten($data);
		$this->assertEqual($result, $data);
	}

	public function testFlattenTwoLevels() {
		$data = array(
			array(
				'Post' => array('id' => '1', 'author_id' => '1', 'title' => 'First Post'),
				'Author' => array('id' => '1', 'user' => 'nate', 'password' => 'foo')
			),
			array(
				'Post' => array(
					'id' => '2',
					'author_id' => '3',
					'title' => 'Second Post',
					'body' => 'Second Post Body'
				),
				'Author' => array('id' => '3', 'user' => 'joel', 'password' => null)
			)
		);

		$expected = array(
			'0.Post.id' => '1', '0.Post.author_id' => '1', '0.Post.title' => 'First Post',
			'0.Author.id' => '1', '0.Author.user' => 'nate', '0.Author.password' => 'foo',
			'1.Post.id' => '2', '1.Post.author_id' => '3', '1.Post.title' => 'Second Post',
			'1.Post.body' => 'Second Post Body', '1.Author.id' => '3',
			'1.Author.user' => 'joel', '1.Author.password' => null
		);
		$result = Set::flatten($data);
		$this->assertEqual($expected, $result);

		$result = Set::expand($result);
		$this->assertEqual($data, $result);

		$result = Set::flatten($data[0], array('separator' => '/'));
		$expected = array(
			'Post/id' => '1', 'Post/author_id' => '1', 'Post/title' => 'First Post',
			'Author/id' => '1', 'Author/user' => 'nate', 'Author/password' => 'foo'
		);
		$this->assertEqual($expected, $result);

		$result = Set::expand($expected, array('separator' => '/'));
		$this->assertEqual($data[0], $result);
	}

	public function testExpand() {
		$data = array(
			'Gallery.Image' => null,
			'Gallery.Image.Tag' => null,
			'Gallery.Image.Tag.Author' => null
		);
		$expected = array('Gallery' => array('Image' => array('Tag' => array('Author' => null))));
		$this->assertEqual($expected, Set::expand($data));

		$data = array(
			'Gallery.Image.Tag' => null,
			'Gallery.Image' => null,
			'Gallery.Image.Tag.Author' => null
		);
		$expected = array('Gallery' => array('Image' => array('Tag' => array('Author' => null))));
		$this->assertEqual($expected, Set::expand($data));

		$data = array(
			'Gallery.Image.Tag.Author' => null,
			'Gallery.Image.Tag' => null,
			'Gallery.Image' => null
		);
		$expected = array('Gallery' => array('Image' => array('Tag' => array('Author' => null))));
		$this->assertEqual($expected, Set::expand($data));
	}

	public function testFormat() {
		$data = array(
			array('Person' => array(
				'first_name' => 'Nate', 'last_name' => 'Abele',
				'city' => 'Queens', 'state' => 'NY', 'something' => '42'
			)),
			array('Person' => array(
				'first_name' => 'Joel', 'last_name' => 'Perras',
				'city' => 'Montreal', 'state' => 'Quebec', 'something' => '{0}'
			)),
			array('Person' => array(
				'first_name' => 'Garrett', 'last_name' => 'Woodworth',
				'city' => 'Venice Beach', 'state' => 'CA', 'something' => '{1}'
			))
		);

		$result = Set::format($data, '{1}, {0}', array('/Person/first_name', '/Person/last_name'));
		$expected = array('Abele, Nate', 'Perras, Joel', 'Woodworth, Garrett');
		$this->assertEqual($expected, $result);

		$result = Set::format($data, '{0}, {1}', array('/Person/last_name', '/Person/first_name'));
		$this->assertEqual($expected, $result);

		$result = Set::format($data, '{0}, {1}', array('/Person/city', '/Person/state'));
		$expected = array('Queens, NY', 'Montreal, Quebec', 'Venice Beach, CA');
		$this->assertEqual($expected, $result);

		$result = Set::format($data, '{{0}, {1}}', array('/Person/city', '/Person/state'));
		$expected = array('{Queens, NY}', '{Montreal, Quebec}', '{Venice Beach, CA}');
		$this->assertEqual($expected, $result);

		$result = Set::format($data, '{{0}, {1}}', array(
			'/Person/something', '/Person/something'
		));
		$expected = array('{42, 42}', '{{0}, {0}}', '{{1}, {1}}');
		$this->assertEqual($expected, $result);

		$result = Set::format($data, '{%2$d, %1$s}', array(
			'/Person/something', '/Person/something'
		));
		$expected = array('{42, 42}', '{0, {0}}', '{0, {1}}');
		$this->assertEqual($expected, $result);

		$result = Set::format($data, '{%1$s, %1$s}', array(
			'/Person/something', '/Person/something'
		));
		$expected = array('{42, 42}', '{{0}, {0}}', '{{1}, {1}}');
		$this->assertEqual($expected, $result);

		$result = Set::format($data, '%2$d, %1$s', array(
			'/Person/first_name', '/Person/something'
		));
		$expected = array('42, Nate', '0, Joel', '0, Garrett');
		$this->assertEqual($expected, $result);

		$result = Set::format($data, '%1$s, %2$d', array(
			'/Person/first_name', '/Person/something'
		));
		$expected = array('Nate, 42', 'Joel, 0', 'Garrett, 0');
		$this->assertEqual($expected, $result);
	}

	public function testMatchesBasic() {
		$a = array(
			array('Article' => array('id' => 1, 'title' => 'Article 1')),
			array('Article' => array('id' => 2, 'title' => 'Article 2')),
			array('Article' => array('id' => 3, 'title' => 'Article 3'))
		);

		$this->assertTrue(Set::matches($a[1]['Article'], array('id=2')));
		$this->assertFalse(Set::matches($a[1]['Article'], array('id>2')));
		$this->assertTrue(Set::matches($a[1]['Article'], array('id>=2')));
		$this->assertFalse(Set::matches($a[1]['Article'], array('id>=3')));
		$this->assertTrue(Set::matches($a[1]['Article'], array('id<=2')));
		$this->assertFalse(Set::matches($a[1]['Article'], array('id<2')));
		$this->assertTrue(Set::matches($a[1]['Article'], array('id>1')));
		$this->assertTrue(Set::matches($a[1]['Article'], array('id>1', 'id<3', 'id!=0')));

		$this->assertTrue(Set::matches(array(), array('3'), 3));
		$this->assertTrue(Set::matches(array(), array('5'), 5));

		$this->assertTrue(Set::matches($a[1]['Article'], array('id')));
		$this->assertTrue(Set::matches($a[1]['Article'], array('id', 'title')));
		$this->assertFalse(Set::matches($a[1]['Article'], array('non-existant')));

		$this->assertTrue(Set::matches($a, '/Article[id=2]'));
		$this->assertFalse(Set::matches($a, '/Article[id=4]'));
		$this->assertTrue(Set::matches($a, array()));
	}

	public function testMatchesMultipleLevels() {
		$result = array(
			'Attachment' => array(
				'keep' => array()
			),
			'Comment' => array(
				'keep' => array('Attachment' => array('fields' => array('attachment')))
			),
			'User' => array('keep' => array()),
			'Article' => array(
				'keep' => array(
					'Comment' => array('fields' => array('comment', 'published')),
					'User' => array('fields' => array('user'))
				)
			)
		);
		$this->assertTrue(Set::matches($result, '/Article/keep/Comment'));

		$result = Set::matches($result, '/Article/keep/Comment/fields/user');
		$this->assertFalse($result);
	}

	public function testExtractReturnsEmptyArray() {
		$expected = array();
		$result = Set::extract(array(), '/Post/id');
		$this->assertIdentical($expected, $result);

		$result = Set::extract(array(
			array('Post' => array('name' => 'bob')),
			array('Post' => array('name' => 'jim'))
		), '/Post/id');
		$this->assertIdentical($expected, $result);

		$result = Set::extract(array(), 'Message.flash');
		$this->assertIdentical($expected, $result);
	}

	public function testExtractionOfNotNull() {
		$data = array(
			'plugin' => null, 'admin' => false, 'controller' => 'posts',
			'action' => 'index', 1, 'whatever'
		);

		$expected = array('controller' => 'posts', 'action' => 'index', 1, 'whatever');
		$result = Set::extract($data, '/');
		$this->assertIdentical($expected, $result);
	}

	public function testExtractOfNumericKeys() {
		$data = array(1, 'whatever');

		$expected = array(1, 'whatever');
		$result = Set::extract($data, '/');
		$this->assertIdentical($expected, $result);
	}

	public function testExtract() {
		$a = array(
			array(
				'Article' => array(
					'id' => '1', 'user_id' => '1', 'title' => 'First Article',
					'body' => 'First Article Body', 'published' => 'Y',
					'created' => '2007-03-18 10:39:23', 'updated' => '2007-03-18 10:41:31'
				),
				'User' => array(
					'id' => '1', 'user' => 'mariano',
					'password' => '5f4dcc3b5aa765d61d8327deb882cf99',
					'created' => '2007-03-17 01:16:23',
					'updated' => '2007-03-17 01:18:31'
				),
				'Comment' => array(
					array(
						'id' => '1', 'article_id' => '1', 'user_id' => '2',
						'comment' => 'First Comment for First Article',
						'published' => 'Y', 'created' => '2007-03-18 10:45:23',
						'updated' => '2007-03-18 10:47:31'
					),
					array(
						'id' => '2', 'article_id' => '1', 'user_id' => '4',
						'comment' => 'Second Comment for First Article', 'published' => 'Y',
						'created' => '2007-03-18 10:47:23',
						'updated' => '2007-03-18 10:49:31'
					)
				),
				'Tag' => array(
					array(
						'id' => '1', 'tag' => 'tag1', 'created' => '2007-03-18 12:22:23',
						'updated' => '2007-03-18 12:24:31'
					),
					array(
						'id' => '2', 'tag' => 'tag2', 'created' => '2007-03-18 12:24:23',
						'updated' => '2007-03-18 12:26:31'
					)
				),
				'Deep' => array(
					'Nesting' => array(
						'test' => array(1 => 'foo', 2 => array('and' => array('more' => 'stuff')))
					)
				)
			),
			array(
				'Article' => array(
					'id' => '3', 'user_id' => '1', 'title' => 'Third Article',
					'body' => 'Third Article Body', 'published' => 'Y',
					'created' => '2007-03-18 10:43:23', 'updated' => '2007-03-18 10:45:31'
				),
				'User' => array(
					'id' => '2', 'user' => 'mariano',
					'password' => '5f4dcc3b5aa765d61d8327deb882cf99',
					'created' => '2007-03-17 01:16:23', 'updated' => '2007-03-17 01:18:31'
				),
				'Comment' => array(),
				'Tag' => array()
			),
			array(
				'Article' => array(
					'id' => '3', 'user_id' => '1', 'title' => 'Third Article',
					'body' => 'Third Article Body', 'published' => 'Y',
					'created' => '2007-03-18 10:43:23', 'updated' => '2007-03-18 10:45:31'
				),
				'User' => array(
					'id' => '3', 'user' => 'mariano',
					'password' => '5f4dcc3b5aa765d61d8327deb882cf99',
					'created' => '2007-03-17 01:16:23', 'updated' => '2007-03-17 01:18:31'
				),
				'Comment' => array(),
				'Tag' => array()
			),
			array(
				'Article' => array(
					'id' => '3', 'user_id' => '1', 'title' => 'Third Article',
					'body' => 'Third Article Body', 'published' => 'Y',
					'created' => '2007-03-18 10:43:23', 'updated' => '2007-03-18 10:45:31'
				),
				'User' => array(
					'id' => '4', 'user' => 'mariano',
					'password' => '5f4dcc3b5aa765d61d8327deb882cf99',
					'created' => '2007-03-17 01:16:23', 'updated' => '2007-03-17 01:18:31'
				),
				'Comment' => array(),
				'Tag' => array()
			),
			array(
				'Article' => array(
					'id' => '3', 'user_id' => '1', 'title' => 'Third Article',
					'body' => 'Third Article Body', 'published' => 'Y',
					'created' => '2007-03-18 10:43:23', 'updated' => '2007-03-18 10:45:31'
				),
				'User' => array(
					'id' => '5', 'user' => 'mariano',
					'password' => '5f4dcc3b5aa765d61d8327deb882cf99',
					'created' => '2007-03-17 01:16:23', 'updated' => '2007-03-17 01:18:31'
				),
				'Comment' => array(),
				'Tag' => array()
			)
		);

		$b = array('Deep' => $a[0]['Deep']);

		$c = array(
			array('a' => array('I' => array('a' => 1))),
			array('a' => array(2)),
			array('a' => array('II' => array('a' => 3, 'III' => array('a' => array('foo' => 4)))))
		);

		$expected = array(array('a' => $c[2]['a']));
		$result = Set::extract($c, '/a/II[a=3]/..');
		$this->assertEqual($expected, $result);

		$expected = array(1, 2, 3, 4, 5);
		$result = Set::extract($a, '/User/id');
		$this->assertEqual($expected, $result);

		$expected = array(1, 2, 3, 4, 5);
		$result = Set::extract($a, '/User/id');
		$this->assertEqual($expected, $result);

		$expected = array(
			array('id' => 1), array('id' => 2), array('id' => 3), array('id' => 4), array('id' => 5)
		);
		$result = Set::extract($a, '/User/id', array('flatten' => false));
		$this->assertEqual($expected, $result);

		$expected = array(array('test' => $a[0]['Deep']['Nesting']['test']));
		$this->assertEqual(Set::extract($a, '/Deep/Nesting/test'), $expected);
		$this->assertEqual(Set::extract($b, '/Deep/Nesting/test'), $expected);

		$expected = array(array('test' => $a[0]['Deep']['Nesting']['test']));
		$result = Set::extract($a, '/Deep/Nesting/test/1/..');
		$this->assertEqual($expected, $result);

		$expected = array(array('test' => $a[0]['Deep']['Nesting']['test']));
		$result = Set::extract($a, '/Deep/Nesting/test/2/and/../..');
		$this->assertEqual($expected, $result);

		$expected = array(array('test' => $a[0]['Deep']['Nesting']['test']));
		$result = Set::extract($a, '/Deep/Nesting/test/2/../../../Nesting/test/2/..');
		$this->assertEqual($expected, $result);

		$expected = array(2);
		$result = Set::extract($a, '/User[2]/id');
		$this->assertEqual($expected, $result);

		$expected = array(4, 5);
		$result = Set::extract($a, '/User[id>3]/id');
		$this->assertEqual($expected, $result);

		$expected = array(2, 3);
		$result = Set::extract($a, '/User[id>1][id<=3]/id');
		$this->assertEqual($expected, $result);

		$expected = array(array('I'), array('II'));
		$result = Set::extract($c, '/a/@*');
		$this->assertEqual($expected, $result);
	}

	public function testExtractWithNonSequentialKeys() {
		$nonSequential = array(
			'User' => array(
				0  => array('id' => 1),
				2  => array('id' => 2),
				6  => array('id' => 3),
				9  => array('id' => 4),
				3  => array('id' => 5)
			)
		);

		$expected = array(1, 2, 3, 4, 5);
		$result = Set::extract($nonSequential, '/User/id');
		$this->assertEqual($expected, $result);
	}

	public function testExtractWithNoZeroKey() {
		$noZero = array(
			'User' => array(
				2  => array('id' => 1),
				4  => array('id' => 2),
				6  => array('id' => 3),
				9  => array('id' => 4),
				3  => array('id' => 5)
			)
		);

		$expected = array(1, 2, 3, 4, 5);
		$result = Set::extract($noZero, '/User/id');
		$this->assertEqual($expected, $result);
	}

	public function testExtractSingle() {

		$single = array('User' => array('id' => 4, 'name' => 'Neo'));

		$expected = array(4);
		$result = Set::extract($single, '/User/id');
		$this->assertEqual($expected, $result);
	}

	public function testExtractHasMany() {
		$tricky = array(
			0 => array('User' => array('id' => 1, 'name' => 'John')),
			1 => array('User' => array('id' => 2, 'name' => 'Bob')),
			2 => array('User' => array('id' => 3, 'name' => 'Tony')),
			'User' => array('id' => 4, 'name' => 'Neo')
		);

		$expected = array(1, 2, 3, 4);
		$result = Set::extract($tricky, '/User/id');
		$this->assertEqual($expected, $result);

		$expected = array(1, 3);
		$result = Set::extract($tricky, '/User[name=/n/]/id');
		$this->assertEqual($expected, $result);

		$expected = array(4);
		$result = Set::extract($tricky, '/User[name=/N/]/id');
		$this->assertEqual($expected, $result);

		$expected = array(1, 3, 4);
		$result = Set::extract($tricky, '/User[name=/N/i]/id');
		$this->assertEqual($expected, $result);

		$expected = array(
			array('id', 'name'), array('id', 'name'), array('id', 'name'), array('id', 'name')
		);
		$result = Set::extract($tricky, '/User/@*');
		$this->assertEqual($expected, $result);
	}

	public function testExtractAssociatedHasMany() {
		$common = array(
			array(
				'Article' => array('id' => 1, 'name' => 'Article 1'),
				'Comment' => array(
					array('id' => 1, 'user_id' => 5, 'article_id' => 1, 'text' => 'Comment 1'),
					array('id' => 2, 'user_id' => 23, 'article_id' => 1, 'text' => 'Comment 2'),
					array('id' => 3, 'user_id' => 17, 'article_id' => 1, 'text' => 'Comment 3')
				)
			),
			array(
				'Article' => array('id' => 2, 'name' => 'Article 2'),
				'Comment' => array(
					array(
						'id' => 4,
						'user_id' => 2,
						'article_id' => 2,
						'text' => 'Comment 4',
						'addition' => ''
					),
					array(
						'id' => 5,
						'user_id' => 23,
						'article_id' => 2,
						'text' => 'Comment 5',
						'addition' => 'foo'
					)
				)
			),
			array(
				'Article' => array('id' => 3, 'name' => 'Article 3'),
				'Comment' => array()
			)
		);
		$result = Set::extract($common, '/');
		$this->assertEqual($result, $common);

		$expected = array(1);
		$result = Set::extract($common, '/Comment/id[:first]');
		$this->assertEqual($expected, $result);

		$expected = array(5);
		$result = Set::extract($common, '/Comment/id[:last]');
		$this->assertEqual($expected, $result);

		$result = Set::extract($common, '/Comment/id');
		$expected = array(1, 2, 3, 4, 5);
		$this->assertEqual($expected, $result);

		$expected = array(1, 2, 4, 5);
		$result = Set::extract($common, '/Comment[id!=3]/id');
		$this->assertEqual($expected, $result);

		$expected = array($common[0]['Comment'][2]);
		$result = Set::extract($common, '/Comment/2');
		$this->assertEqual($expected, $result);

		$expected = array($common[0]['Comment'][0]);
		$result = Set::extract($common, '/Comment[1]/.[id=1]');
		$this->assertEqual($expected, $result);

		$expected = array($common[1]['Comment'][1]);
		$result = Set::extract($common, '/1/Comment/.[2]');
		$this->assertEqual($expected, $result);

		$expected = array(array('Comment' => $common[1]['Comment'][0]));
		$result = Set::extract($common, '/Comment[addition=]');
		$this->assertEqual($expected, $result);

		$expected = array(3);
		$result = Set::extract($common, '/Article[:last]/id');
		$this->assertEqual($expected, $result);

		$expected = array();
		$result = Set::extract(array(), '/User/id');
		$this->assertEqual($expected, $result);
	}

	public function testExtractHabtm() {
		$habtm = array(
			array(
				'Post' => array('id' => 1, 'title' => 'great post'),
				'Comment' => array(
					array('id' => 1, 'text' => 'foo', 'User' => array('id' => 1, 'name' => 'bob')),
					array('id' => 2, 'text' => 'bar', 'User' => array('id' => 2, 'name' => 'tod'))
				)
			),
			array(
				'Post' => array('id' => 2, 'title' => 'fun post'),
				'Comment' => array(
					array('id' => 3, 'text' => '123', 'User' => array('id' => 3, 'name' => 'dan')),
					array('id' => 4, 'text' => '987', 'User' => array('id' => 4, 'name' => 'jim'))
				)
			)
		);

		$result = Set::extract($habtm, '/Comment/User[name=/\w+/]/..');
		$this->assertEqual(count($result), 4);
		$this->assertEqual($result[0]['Comment']['User']['name'], 'bob');
		$this->assertEqual($result[1]['Comment']['User']['name'], 'tod');
		$this->assertEqual($result[2]['Comment']['User']['name'], 'dan');
		$this->assertEqual($result[3]['Comment']['User']['name'], 'jim');

		$result = Set::extract($habtm, '/Comment/User[name=/[a-z]+/]/..');
		$this->assertEqual(count($result), 4);
		$this->assertEqual($result[0]['Comment']['User']['name'], 'bob');
		$this->assertEqual($result[1]['Comment']['User']['name'], 'tod');
		$this->assertEqual($result[2]['Comment']['User']['name'], 'dan');
		$this->assertEqual($result[3]['Comment']['User']['name'], 'jim');

		$result = Set::extract($habtm, '/Comment/User[name=/bob|dan/]/..');
		$this->assertEqual(count($result), 2);
		$this->assertEqual($result[0]['Comment']['User']['name'], 'bob');
		$this->assertEqual($result[1]['Comment']['User']['name'], 'dan');

		$result = Set::extract($habtm, '/Comment/User[name=/bob|tod/]/..');
		$this->assertEqual(count($result), 2);
		$this->assertEqual($result[0]['Comment']['User']['name'], 'bob');
		$this->assertEqual($result[1]['Comment']['User']['name'], 'tod');
	}

	public function testExtractFromTree() {
		$tree = array(
			array(
				'Category' => array('name' => 'Category 1'),
				'children' => array(array('Category' => array('name' => 'Category 1.1')))
			),
			array(
				'Category' => array('name' => 'Category 2'),
				'children' => array(
					array('Category' => array('name' => 'Category 2.1')),
					array('Category' => array('name' => 'Category 2.2'))
				)
			),
			array(
				'Category' => array('name' => 'Category 3'),
				'children' => array(array('Category' => array('name' => 'Category 3.1')))
			)
		);

		$expected = array(array('Category' => $tree[1]['Category']));
		$result = Set::extract($tree, '/Category[name=Category 2]');
		$this->assertEqual($expected, $result);

		$expected = array(array(
			'Category' => $tree[1]['Category'], 'children' => $tree[1]['children']
		));
		$result = Set::extract($tree, '/Category[name=Category 2]/..');
		$this->assertEqual($expected, $result);

		$expected = array(
			array('children' => $tree[1]['children'][0]),
			array('children' => $tree[1]['children'][1])
		);
		$result = Set::extract($tree, '/Category[name=Category 2]/../children');
		$this->assertEqual($expected, $result);
	}

	public function testExtractOnMixedKeys() {
		$mixedKeys = array(
			'User' => array(
				0 => array('id' => 4, 'name' => 'Neo'),
				1 => array('id' => 5, 'name' => 'Morpheus'),
				'stringKey' => array()
			)
		);

		$expected = array('Neo', 'Morpheus');
		$result = Set::extract($mixedKeys, '/User/name');
		$this->assertEqual($expected, $result);
	}

	public function testExtractSingleWithNameCondition() {
		$single = array(
			array('CallType' => array('name' => 'Internal Voice'), 'x' => array('hour' => 7))
		);

		$expected = array(7);
		$result = Set::extract($single, '/CallType[name=Internal Voice]/../x/hour');
		$this->assertEqual($expected, $result);
	}

	public function testExtractWithNameCondition() {
		$multiple = array(
			array('CallType' => array('name' => 'Internal Voice'), 'x' => array('hour' => 7)),
			array('CallType' => array('name' => 'Internal Voice'), 'x' => array('hour' => 2)),
			array('CallType' => array('name' => 'Internal Voice'), 'x' => array('hour' => 1))
		);

		$expected = array(7, 2, 1);
		$result = Set::extract($multiple, '/CallType[name=Internal Voice]/../x/hour');
		$this->assertEqual($expected, $result);
	}

	public function testExtractWithTypeCondition() {
		$f = array(
			array(
				'file' => array(
					'name' => 'zipfile.zip',
					'type' => 'application/zip',
					'tmp_name' => '/tmp/php178.tmp',
					'error' => 0,
					'size' => '564647'
				)
			),
			array(
				'file' => array(
					'name' => 'zipfile2.zip',
					'type' => 'application/x-zip-compressed',
					'tmp_name' => '/tmp/php179.tmp',
					'error' => 0,
					'size' => '354784'
				)
			),
			array(
				'file' => array(
					'name' => 'picture.jpg',
					'type' => 'image/jpeg',
					'tmp_name' => '/tmp/php180.tmp',
					'error' => 0,
					'size' => '21324'
				)
			)
		);
		$expected = array(array(
			'name' => 'zipfile2.zip', 'type' => 'application/x-zip-compressed',
			'tmp_name' => '/tmp/php179.tmp', 'error' => 0, 'size' => '354784'
		));
		$result = Set::extract($f, '/file/.[type=application/x-zip-compressed]');
		$this->assertEqual($expected, $result);

		$expected = array(array(
			'name' => 'zipfile.zip', 'type' => 'application/zip',
			'tmp_name' => '/tmp/php178.tmp', 'error' => 0, 'size' => '564647'
		));
		$result = Set::extract($f, '/file/.[type=application/zip]');
		$this->assertEqual($expected, $result);

	}

	public function testIsNumericArrayCheck() {
		$data = array('one');
		$this->assertTrue(Set::isNumeric(array_keys($data)));

		$data = array(1 => 'one');
		$this->assertFalse(Set::isNumeric($data));

		$data = array('one');
		$this->assertFalse(Set::isNumeric($data));

		$data = array('one' => 'two');
		$this->assertFalse(Set::isNumeric($data));

		$data = array('one' => 1);
		$this->assertTrue(Set::isNumeric($data));

		$data = array(0);
		$this->assertTrue(Set::isNumeric($data));

		$data = array('one', 'two', 'three', 'four', 'five');
		$this->assertTrue(Set::isNumeric(array_keys($data)));

		$data = array(1 => 'one', 2 => 'two', 3 => 'three', 4 => 'four', 5 => 'five');
		$this->assertTrue(Set::isNumeric(array_keys($data)));

		$data = array('1' => 'one', 2 => 'two', 3 => 'three', 4 => 'four', 5 => 'five');
		$this->assertTrue(Set::isNumeric(array_keys($data)));

		$data = array('one', 2 => 'two', 3 => 'three', 4 => 'four', 'a' => 'five');
		$this->assertFalse(Set::isNumeric(array_keys($data)));

		$data = array();
		$this->assertNull(Set::isNumeric($data));
	}

	public function testCheckKeys() {
		$data = array('Multi' => array('dimensonal' => array('array')));
		$this->assertTrue(Set::check($data, 'Multi.dimensonal'));
		$this->assertFalse(Set::check($data, 'Multi.dimensonal.array'));

		$data = array(
			array(
				'Article' => array(
					'id' => '1', 'user_id' => '1', 'title' => 'First Article',
					'body' => 'First Article Body', 'published' => 'Y',
					'created' => '2007-03-18 10:39:23',
					'updated' => '2007-03-18 10:41:31'
				),
				'User' => array(
					'id' => '1', 'user' => 'mariano',
					'password' => '5f4dcc3b5aa765d61d8327deb882cf99',
					'created' => '2007-03-17 01:16:23',
					'updated' => '2007-03-17 01:18:31'
				),
				'Comment' => array(
					array(
						'id' => '1', 'article_id' => '1', 'user_id' => '2',
						'comment' => 'First Comment for First Article',
						'published' => 'Y', 'created' => '2007-03-18 10:45:23',
						'updated' => '2007-03-18 10:47:31'
					),
					array(
						'id' => '2', 'article_id' => '1', 'user_id' => '4',
						'comment' => 'Second Comment for First Article',
						'published' => 'Y', 'created' => '2007-03-18 10:47:23',
						'updated' => '2007-03-18 10:49:31'
					)
				),
				'Tag' => array(
					array(
						'id' => '1', 'tag' => 'tag1', 'created' => '2007-03-18 12:22:23',
						'updated' => '2007-03-18 12:24:31'
					),
					array(
						'id' => '2', 'tag' => 'tag2', 'created' => '2007-03-18 12:24:23',
						'updated' => '2007-03-18 12:26:31'
					)
				)
			),
			array(
				'Article' => array(
					'id' => '3', 'user_id' => '1', 'title' => 'Third Article',
					'body' => 'Third Article Body', 'published' => 'Y',
					'created' => '2007-03-18 10:43:23', 'updated' => '2007-03-18 10:45:31'
				),
				'User' => array(
					'id' => '1', 'user' => 'mariano',
					'password' => '5f4dcc3b5aa765d61d8327deb882cf99',
					'created' => '2007-03-17 01:16:23', 'updated' => '2007-03-17 01:18:31'
				),
				'Comment' => array(),
				'Tag' => array()
			)
		);
		$this->assertTrue(Set::check($data, '0.Article.user_id'));
		$this->assertTrue(Set::check($data, '0.Comment.0.id'));
		$this->assertFalse(Set::check($data, '0.Comment.0.id.0'));
		$this->assertTrue(Set::check($data, '0.Article.user_id'));
		$this->assertFalse(Set::check($data, '0.Article.user_id.a'));
	}

	public function testMerge() {
		$result = Set::merge(array('foo'), array());
		$this->assertIdentical($result, array('foo'));

		$result = Set::merge((array) 'foo', (array) 'bar');
		$this->assertIdentical($result, array('foo', 'bar'));

		$result = Set::merge((array) 'foo', array('user' => 'bob', 'no-bar'));
		$this->assertIdentical($result, array('foo', 'user' => 'bob', 'no-bar'));

		$a = array('foo', 'foo2');
		$b = array('bar', 'bar2');
		$this->assertIdentical(Set::merge($a, $b), array('foo', 'foo2', 'bar', 'bar2'));

		$a = array('foo' => 'bar', 'bar' => 'foo');
		$b = array('foo' => 'no-bar', 'bar' => 'no-foo');
		$this->assertIdentical(Set::merge($a, $b), array('foo' => 'no-bar', 'bar' => 'no-foo'));

		$a = array('users' => array('bob', 'jim'));
		$b = array('users' => array('lisa', 'tina'));
		$this->assertIdentical(
			Set::merge($a, $b), array('users' => array('bob', 'jim', 'lisa', 'tina'))
		);

		$a = array('users' => array('jim', 'bob'));
		$b = array('users' => 'none');
		$this->assertIdentical(Set::merge($a, $b), array('users' => 'none'));

		$a = array('users' => array('lisa' => array('id' => 5, 'pw' => 'secret')), 'lithium');
		$b = array('users' => array('lisa' => array('pw' => 'new-pass', 'age' => 23)), 'ice-cream');
		$this->assertIdentical(
			Set::merge($a, $b),
			array(
				'users' => array('lisa' => array('id' => 5, 'pw' => 'new-pass', 'age' => 23)),
				'lithium',
				'ice-cream'
			)
		);

		$c = array(
			'users' => array(
				'lisa' => array('pw' => 'you-will-never-guess', 'age' => 25, 'pet' => 'dog')
			),
			'chocolate'
		);
		$expected = array(
			'users' => array(
				'lisa' => array(
					'id' => 5, 'pw' => 'you-will-never-guess', 'age' => 25, 'pet' => 'dog'
				)
			),
			'lithium',
			'ice-cream',
			'chocolate'
		);
		$this->assertIdentical($expected, Set::merge(Set::merge($a, $b), $c));

		$this->assertIdentical($expected, Set::merge(Set::merge($a, $b), Set::merge(array(), $c)));

		$result = Set::merge($a, Set::merge($b, $c));
		$this->assertIdentical($expected, $result);

		$a = array('Tree', 'CounterCache', 'Upload' => array(
			'folder' => 'products', 'fields' => array(
				'image_1_id', 'image_2_id', 'image_3_id', 'image_4_id', 'image_5_id'
			)
		));
		$b =  array(
			'Cacheable' => array('enabled' => false),
			'Limit', 'Bindable', 'Validator', 'Transactional'
		);

		$expected = array('Tree', 'CounterCache', 'Upload' => array(
			'folder' => 'products', 'fields' => array(
				'image_1_id', 'image_2_id', 'image_3_id', 'image_4_id', 'image_5_id'
			)),
			'Cacheable' => array('enabled' => false),
			'Limit',
			'Bindable',
			'Validator',
			'Transactional'
		);
		$this->assertIdentical(Set::merge($a, $b), $expected);

		$expected = array('Tree' => null, 'CounterCache' => null, 'Upload' => array(
			'folder' => 'products', 'fields' => array(
				'image_1_id', 'image_2_id', 'image_3_id', 'image_4_id', 'image_5_id'
			)),
			'Cacheable' => array('enabled' => false),
			'Limit' => null,
			'Bindable' => null,
			'Validator' => null,
			'Transactional' => null
		);
		$this->assertIdentical(Set::normalize(Set::merge($a, $b)), $expected);
	}

	public function testSort() {
		$a = array(
			array('Person' => array('name' => 'Jeff'), 'Friend' => array(array('name' => 'Nate'))),
			array('Person' => array('name' => 'Tracy'), 'Friend' => array(
				array('name' => 'Lindsay')
			))
		);
		$b = array(
			array('Person' => array('name' => 'Tracy'),'Friend' => array(
				array('name' => 'Lindsay')
			)),
			array('Person' => array('name' => 'Jeff'), 'Friend' => array(array('name' => 'Nate')))
		);
		$a = Set::sort($a, '/Friend/name', 'asc');
		$this->assertIdentical($a, $b);

		$b = array(
			array('Person' => array('name' => 'Jeff'), 'Friend' => array(array('name' => 'Nate'))),
			array('Person' => array('name' => 'Tracy'), 'Friend' => array(
				array('name' => 'Lindsay')
			))
		);
		$a = array(
			array('Person' => array('name' => 'Tracy'), 'Friend' => array(
				array('name' => 'Lindsay')
			)),
			array('Person' => array('name' => 'Jeff'), 'Friend' => array(array('name' => 'Nate')))
		);
		$a = Set::sort($a, '/Friend/name', 'desc');
		$this->assertIdentical($a, $b);

		$a = array(
			array('Person' => array('name' => 'Jeff'), 'Friend' => array(array('name' => 'Nate'))),
			array('Person' => array('name' => 'Tracy'), 'Friend' => array(
				array('name' => 'Lindsay')
			)),
			array('Person' => array('name' => 'Adam'), 'Friend' => array(array('name' => 'Bob')))
		);
		$b = array(
			array('Person' => array('name' => 'Adam'),'Friend' => array(array('name' => 'Bob'))),
			array('Person' => array('name' => 'Jeff'), 'Friend' => array(array('name' => 'Nate'))),
			array('Person' => array('name' => 'Tracy'), 'Friend' => array(
				array('name' => 'Lindsay')
			))
		);
		$a = Set::sort($a, '/Person/name', 'asc');
		$this->assertIdentical($a, $b);

		$a = array(array(7, 6, 4), array(3, 4, 5), array(3, 2, 1));
		$b = array(array(3, 2, 1), array(3, 4, 5), array(7, 6, 4));

		$a = Set::sort($a, '/', 'asc');
		$this->assertIdentical($a, $b);

		$a = array(array(7, 6, 4), array(3, 4, 5), array(3, 2, array(1, 1, 1)));
		$b = array(array(3, 2, array(1, 1, 1)), array(3, 4, 5), array(7, 6, 4));

		$a = Set::sort($a, '/.', 'asc');
		$this->assertIdentical($a, $b);

		$a = array(
			array('Person' => array('name' => 'Jeff')),
			array('Shirt' => array('color' => 'black'))
		);
		$b = array(array('Person' => array('name' => 'Jeff')));
		$a = Set::sort($a, '/Person/name', 'asc');
		$this->assertIdentical($a, $b);
	}

	public function testInsert() {
		$a = array('pages' => array('name' => 'page'));

		$result = Set::insert($a, 'files', array('name' => 'files'));
		$expected = array('pages' => array('name' => 'page'), 'files' => array('name' => 'files'));
		$this->assertIdentical($expected, $result);

		$a = array('pages' => array('name' => 'page'));
		$result = Set::insert($a, 'pages.name', array());
		$expected = array('pages' => array('name' => array()));
		$this->assertIdentical($expected, $result);

		$a = array('pages' => array(array('name' => 'main'), array('name' => 'about')));

		$result = Set::insert($a, 'pages.1.vars', array('title' => 'page title'));
		$expected = array(
			'pages' => array(
				array('name' => 'main'),
				array('name' => 'about', 'vars' => array('title' => 'page title'))
			)
		);
		$this->assertIdentical($expected, $result);
	}

	public function testRemove() {
		$a = array('pages' => array('name' => 'page'), 'files' => array('name' => 'files'));

		$result = Set::remove($a, 'files', array('name' => 'files'));
		$expected = array('pages' => array('name' => 'page'));
		$this->assertIdentical($expected, $result);

		$a = array(
			'pages' => array(
				array('name' => 'main'),
				array('name' => 'about', 'vars' => array('title' => 'page title'))
			)
		);

		$result = Set::remove($a, 'pages.1.vars', array('title' => 'page title'));
		$expected = array('pages' => array(array('name' => 'main'), array('name' => 'about')));
		$this->assertIdentical($expected, $result);

		$result = Set::remove($a, 'pages.2.vars', array('title' => 'page title'));
		$expected = $a;
		$this->assertIdentical($expected, $result);
	}

	public function testCheck() {
		$set = array('My Index 1' => array(
			'First' => 'The first item'
		));
		$result = Set::check($set, 'My Index 1.First');
		$this->assertTrue($result);

		$this->assertTrue(Set::check($set, 'My Index 1'));
		$this->assertTrue(Set::check($set, array()));

		$set = array('My Index 1' => array('First' => array('Second' => array('Third' => array(
			'Fourth' => 'Heavy. Nesting.'
		)))));
		$this->assertTrue(Set::check($set, 'My Index 1.First.Second'));
		$this->assertTrue(Set::check($set, 'My Index 1.First.Second.Third'));
		$this->assertTrue(Set::check($set, 'My Index 1.First.Second.Third.Fourth'));
		$this->assertFalse(Set::check($set, 'My Index 1.First.Seconds.Third.Fourth'));
	}

	public function testInsertAndRemoveWithFunkyKeys() {
		$set = Set::insert(array(), 'Session Test', "test");
		$result = Set::extract($set, '/Session Test');
		$this->assertEqual($result, array('test'));

		$set = Set::remove($set, 'Session Test');
		$this->assertFalse(Set::check($set, 'Session Test'));

		$this->assertTrue($set = Set::insert(array(), 'Session Test.Test Case', "test"));
		$this->assertTrue(Set::check($set, 'Session Test.Test Case'));
	}

	public function testDiff() {
		$a = array(array('name' => 'main'), array('name' => 'about'));
		$b = array(array('name' => 'main'), array('name' => 'about'), array('name' => 'contact'));

		$result = Set::diff($a, $b);
		$expected = array(2 => array('name' => 'contact'));
		$this->assertIdentical($expected, $result);

		$result = Set::diff($a, array());
		$expected = $a;
		$this->assertIdentical($expected, $result);

		$result = Set::diff(array(), $b);
		$expected = $b;
		$this->assertIdentical($expected, $result);

		$b = array(array('name' => 'me'), array('name' => 'about'));

		$result = Set::diff($a, $b);
		$expected = array(array('name' => 'main'));
		$this->assertIdentical($expected, $result);
	}

	public function testContains() {
		$a = array(
			0 => array('name' => 'main'),
			1 => array('name' => 'about')
		);
		$b = array(
			0 => array('name' => 'main'),
			1 => array('name' => 'about'),
			2 => array('name' => 'contact'),
			'a' => 'b'
		);

		$this->assertTrue(Set::contains($a, $a));
		$this->assertFalse(Set::contains($a, $b));
		$this->assertTrue(Set::contains($b, $a));
	}

	public function testCombine() {
		$result = Set::combine(array(), '/User/id', '/User/Data');
		$this->assertFalse($result);
		$result = Set::combine('', '/User/id', '/User/Data');
		$this->assertFalse($result);

		$a = array(
			array('User' => array('id' => 2, 'group_id' => 1,
				'Data' => array('user' => 'mariano.iglesias','name' => 'Mariano Iglesias'))),
			array('User' => array('id' => 14, 'group_id' => 2,
				'Data' => array('user' => 'jperras', 'name' => 'Joel Perras'))),
			array('User' => array('id' => 25, 'group_id' => 1,
				'Data' => array('user' => 'gwoo','name' => 'The Gwoo'))));
		$result = Set::combine($a, '/User/id');
		$expected = array(2 => null, 14 => null, 25 => null);
		$this->assertIdentical($expected, $result);

		$result = Set::combine($a, '/User/id', '/User/non-existant');
		$expected = array(2 => null, 14 => null, 25 => null);
		$this->assertIdentical($expected, $result);

		$result = Set::combine($a, '/User/id', '/User/Data/.');
		$expected = array(
			2 => array('user' => 'mariano.iglesias', 'name' => 'Mariano Iglesias'),
			14 => array('user' => 'jperras', 'name' => 'Joel Perras'),
			25 => array('user' => 'gwoo', 'name' => 'The Gwoo'));
		$this->assertIdentical($expected, $result);

		$result = Set::combine($a, '/User/id', '/User/Data/name/.');
		$expected = array(
			2 => 'Mariano Iglesias',
			14 => 'Joel Perras',
			25 => 'The Gwoo');
		$this->assertIdentical($expected, $result);

		$result = Set::combine($a, '/User/id', '/User/Data/.', '/User/group_id');
		$expected = array(
			1 => array(
				2 => array('user' => 'mariano.iglesias', 'name' => 'Mariano Iglesias'),
				25 => array('user' => 'gwoo', 'name' => 'The Gwoo')),
			2 => array(
				14 => array('user' => 'jperras', 'name' => 'Joel Perras')));
		$this->assertIdentical($expected, $result);

		$result = Set::combine($a, '/User/id', '/User/Data/name/.', '/User/group_id');
		$expected = array(
			1 => array(
				2 => 'Mariano Iglesias',
				25 => 'The Gwoo'),
			2 => array(
				14 => 'Joel Perras'));
		$this->assertIdentical($expected, $result);

		$result = Set::combine(
			$a,
			'/User/id',
			array('{0}: {1}', '/User/Data/user', '/User/Data/name'),
			'/User/group_id'
		);
		$expected = array(
			1 => array(2 => 'mariano.iglesias: Mariano Iglesias', 25 => 'gwoo: The Gwoo'),
			2 => array(14 => 'jperras: Joel Perras')
		);
		$this->assertIdentical($expected, $result);

		$result = Set::combine(
			$a,
			array('{0}: {1}', '/User/Data/user', '/User/Data/name'),
			'/User/id'
		);
		$expected = array(
			'mariano.iglesias: Mariano Iglesias' => 2,
			'jperras: Joel Perras' => 14,
			'gwoo: The Gwoo' => 25
		);
		$this->assertIdentical($expected, $result);

		$result = Set::combine(
			$a,
			array('{1}: {0}', '/User/Data/user', '/User/Data/name'),
			'/User/id'
		);
		$expected = array(
			'Mariano Iglesias: mariano.iglesias' => 2,
			'Joel Perras: jperras' => 14,
			'The Gwoo: gwoo' => 25
		);
		$this->assertIdentical($expected, $result);

		$result = Set::combine($a, array(
			'%1$s: %2$d', '/User/Data/user', '/User/id'), '/User/Data/name'
		);
		$expected = array(
			'mariano.iglesias: 2' => 'Mariano Iglesias',
			'jperras: 14' => 'Joel Perras',
			'gwoo: 25' => 'The Gwoo'
		);
		$this->assertIdentical($expected, $result);

		$result = Set::combine($a, array(
			'%2$d: %1$s', '/User/Data/user', '/User/id'), '/User/Data/name'
		);
		$expected = array(
			'2: mariano.iglesias' => 'Mariano Iglesias',
			'14: jperras' => 'Joel Perras',
			'25: gwoo' => 'The Gwoo'
		);
		$this->assertIdentical($expected, $result);

		$b = new stdClass();
		$b->users = array(
			array('User' => array(
				'id' => 2, 'group_id' => 1, 'Data' => array(
					'user' => 'mariano.iglesias','name' => 'Mariano Iglesias'
				)
			)),
			array('User' => array('id' => 14, 'group_id' => 2, 'Data' => array(
				'user' => 'jperras', 'name' => 'Joel Perras'
			))),
			array('User' => array('id' => 25, 'group_id' => 1, 'Data' => array(
				'user' => 'gwoo','name' => 'The Gwoo'
			)))
		);
		$result = Set::combine($b, '/users/User/id');
		$expected = array(2 => null, 14 => null, 25 => null);
		$this->assertIdentical($expected, $result);

		$result = Set::combine($b, '/users/User/id', '/users/User/non-existant');
		$expected = array(2 => null, 14 => null, 25 => null);
		$this->assertIdentical($expected, $result);
	}

	public function testAppend() {
		$array1 = array('ModelOne' => array(
			'id' => 1001, 'field_one' => 'a1.m1.f1', 'field_two' => 'a1.m1.f2'
		));
		$array2 = array('ModelTwo' => array(
			'id' => 1002, 'field_one' => 'a2.m2.f1', 'field_two' => 'a2.m2.f2'
		));

		$result = Set::append($array1, $array2);

		$this->assertIdentical($result, $array1 + $array2);

		$array3 = array('ModelOne' => array(
			'id' => 1003, 'field_one' => 'a3.m1.f1',
			'field_two' => 'a3.m1.f2', 'field_three' => 'a3.m1.f3'
		));
		$result = Set::append($array1, $array3);

		$expected = array('ModelOne' => array(
			'id' => 1001, 'field_one' => 'a1.m1.f1',
			'field_two' => 'a1.m1.f2', 'field_three' => 'a3.m1.f3'
		));
		$this->assertIdentical($expected, $result);

		$array1 = array(
			array('ModelOne' => array(
				'id' => 1001, 'field_one' => 's1.0.m1.f1', 'field_two' => 's1.0.m1.f2'
			)),
			array('ModelTwo' => array(
				'id' => 1002, 'field_one' => 's1.1.m2.f2', 'field_two' => 's1.1.m2.f2'
			))
		);
		$array2 = array(
			array('ModelOne' => array(
				'id' => 1001, 'field_one' => 's2.0.m1.f1', 'field_two' => 's2.0.m1.f2'
			)),
			array('ModelTwo' => array(
				'id' => 1002, 'field_one' => 's2.1.m2.f2', 'field_two' => 's2.1.m2.f2'
			))
		);

		$result = Set::append($array1, $array2);
		$this->assertIdentical($result, $array1);

		$array3 = array(array('ModelThree' => array(
			'id' => 1003, 'field_one' => 's3.0.m3.f1', 'field_two' => 's3.0.m3.f2'
		)));

		$result = Set::append($array1, $array3);
		$expected = array(
			array(
				'ModelOne' => array(
					'id' => 1001, 'field_one' => 's1.0.m1.f1', 'field_two' => 's1.0.m1.f2'
				),
				'ModelThree' => array(
					'id' => 1003, 'field_one' => 's3.0.m3.f1', 'field_two' => 's3.0.m3.f2'
				)
			),
			array('ModelTwo' => array(
				'id' => 1002, 'field_one' => 's1.1.m2.f2', 'field_two' => 's1.1.m2.f2'
			))
		);
		$this->assertIdentical($expected, $result);

		$result = Set::append($array1, array());
		$this->assertIdentical($result, $array1);

		$result = Set::append($array1, $array2);
		$this->assertIdentical($result, $array1 + $array2);

		$result = Set::append(array(), array('2'));
		$this->assertIdentical(array('2'), $result);

		$array1 = array(
			'ModelOne' => array(
				'id' => 1001, 'field_one' => 's1.0.m1.f1', 'field_two' => 's1.0.m1.f2'
			),
			'ModelTwo' => array(
				'id' => 1002, 'field_one' => 's1.0.m2.f1', 'field_two' => 's1.0.m2.f2'
			)
		);
		$array2 = array(
			'ModelTwo' => array(
				'field_three' => 's1.0.m2.f3'
			)
		);
		$array3 = array(
			'ModelOne' => array(
				'field_three' => 's1.0.m1.f3'
			)
		);

		$result = Set::append($array1, $array2, $array3);

		$expected = array(
			'ModelOne' => array(
				'id' => 1001,
				'field_one' => 's1.0.m1.f1',
				'field_two' => 's1.0.m1.f2',
				'field_three' => 's1.0.m1.f3'
			),
			'ModelTwo' => array(
				'id' => 1002,
				'field_one' => 's1.0.m2.f1',
				'field_two' => 's1.0.m2.f2',
				'field_three' => 's1.0.m2.f3'
			)
		);
		$this->assertIdentical($expected, $result);
	}

	public function testStrictKeyCheck() {
		$set = array('a' => 'hi');
		$this->assertFalse(Set::check($set, 'a.b'));
		$this->assertTrue(Set::check($set, 'a'));
	}

	public function testMixedKeyNormalization() {
		$input = array('"string"' => array('before' => '=>'), 1 => array('before' => '=>'));
		$result = Set::normalize($input);
		$this->assertEqual($input, $result);

		$input = 'Foo,Bar,Baz';
		$result = Set::normalize($input);
		$this->assertEqual(array('Foo' => null, 'Bar' => null, 'Baz' => null), $result);

		$input = array('baz' => 'foo', 'bar');
		$result = Set::normalize($input, false);
		$this->assertEqual(array('baz' => 'foo', 'bar' => null), $result);
	}

	public function testSetSlice() {
		$data = array('key1' => 'val1', 'key2' => 'val2', 'key3' => 'val3');
		list($kept, $removed) = Set::slice($data, array('key3'));
		$this->assertEqual(array('key3' => 'val3'), $removed);
		$this->assertEqual(array('key1' => 'val1', 'key2' => 'val2'), $kept);

		$data = array('key1' => 'val1', 'key2' => 'val2', 'key3' => 'val3');
		list($kept, $removed) = Set::slice($data, array('key1', 'key3'));
		$this->assertEqual(array('key1' => 'val1', 'key3' => 'val3'), $removed);
		$this->assertEqual(array('key2' => 'val2'), $kept);

		$data = array('key1' => 'val1', 'key2' => 'val2', 'key3' => 'val3');
		list($kept, $removed) = Set::slice($data, 'key2');
		$this->assertEqual(array('key2' => 'val2'), $removed);
		$this->assertEqual(array('key1' => 'val1', 'key3' => 'val3'), $kept);

		$data = array('key1' => 'val1', 'key2' => 'val2', 'key3' => array('foo' => 'bar'));
		list($kept, $removed) = Set::slice($data, array('key1', 'key3'));
		$this->assertEqual(array('key1' => 'val1', 'key3' => array('foo' => 'bar')), $removed);
		$this->assertEqual(array('key2' => 'val2'), $kept);
	}
}

?>