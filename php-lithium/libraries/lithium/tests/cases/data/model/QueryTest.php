<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\cases\data\model;

use lithium\data\model\Query;
use lithium\data\entity\Record;
use lithium\tests\mocks\data\MockPostObject;
use lithium\tests\mocks\data\model\MockDatabase;
use lithium\tests\mocks\data\model\MockQueryPost;
use lithium\tests\mocks\data\model\MockQueryComment;

class QueryTest extends \lithium\test\Unit {

	protected $_model = 'lithium\tests\mocks\data\model\MockQueryPost';

	protected $_configs = array();

	protected $_queryArr = array(
		'model' => 'lithium\tests\mocks\data\model\MockQueryPost',
		'type' => 'read',
		'order' => 'created DESC',
		'limit' => 10,
		'page' => 1,
		'fields' => array('id', 'author_id', 'title'),
		'conditions' => array('author_id' => 12),
		'comment' => 'Find all posts by author 12'
	);

	public function setUp() {
		$this->db = new MockDatabase();
		MockQueryPost::$connection = $this->db;
		MockQueryComment::$connection = $this->db;
	}

	public function tearDown() {
		MockQueryPost::reset();
		MockQueryComment::reset();
	}

	/**
	 * Tests that configuration settings are delegating to matching method names
	 */
	public function testObjectConstruction() {
		$query = new Query();
		$this->assertFalse($query->conditions());

		$query = new Query(array('conditions' => 'foo', 'limit' => '10'));
		$this->assertEqual(array('foo'), $query->conditions());
		$this->assertIdentical(10, $query->limit());
	}

	public function testModel() {
		$query = new Query($this->_queryArr);
		$this->assertEqual($this->_model, $query->model());

		$query->model('lithium\tests\mocks\data\model\MockQueryComment');

		$expected = 'lithium\tests\mocks\data\model\MockQueryComment';
		$result = $query->model();
		$this->assertEqual($expected, $result);
	}

	public function testFields() {
		$query = new Query($this->_queryArr);

		$expected = array('id','author_id','title');
		$result = $query->fields();
		$this->assertEqual($expected, $result);

		$query->fields('content');

		$expected = array('id','author_id','title','content');
		$result = $query->fields();
		$this->assertEqual($expected, $result);

		$query->fields(array('updated','created'));

		$expected = array('id','author_id','title','content','updated','created');
		$result = $query->fields();
		$this->assertEqual($expected, $result);

		$query->fields(false);
		$query->fields(array('id', 'title'));

		$expected = array('id','title');
		$result = $query->fields();
		$this->assertEqual($expected, $result);

		$query->fields(false);
		$expected = array(
			array(array('count(MockDatabasePost.id)')),
			array('count(MockDatabasePost.id)'),
			array((object) 'count(MockDatabasePost.id)'),
			(object) 'count(MockDatabasePost.id)'
		);
		$query->fields($expected);
		$result = $query->fields();
		$this->assertEqual($expected, $result);
	}

	public function testFieldsWithArray() {
		$query = new Query(array(
			'model' => 'lithium\tests\mocks\data\model\MockQueryPost',
			'type' => 'read',
			'with' => 'MockQueryComment'
		));

		$query->fields(array('MockQueryPost', 'MockQueryPost' => array('id')));
		$result = $query->fields();
		$expected = array('MockQueryPost', 'MockQueryPost.id');
		$this->assertEqual($expected, $result);

		$query->fields(false);
		$query->fields(array(
			'MockQueryPost' => array('id'),
			'title',
			'MockQueryComment' => array('comment', 'title'),
			'MockQueryComment'
		));
		$result = $query->fields();
		$expected = array(
			'MockQueryPost.id',
			'title',
			'MockQueryComment.comment',
			'MockQueryComment.title',
			'MockQueryComment'
		);
		$this->assertEqual($expected, $result);
	}

	public function testLimit() {
		$query = new Query($this->_queryArr);

		$expected = 10;
		$result = $query->limit();
		$this->assertEqual($expected, $result);

		$query->limit(5);

		$expected = 5;
		$result = $query->limit();
		$this->assertEqual($expected, $result);

		$query->limit(false);
		$this->assertNull($query->limit());
	}

	public function testPage() {
		$query = new Query($this->_queryArr);

		$expected = 1;
		$result = $query->page();
		$this->assertEqual($expected, $result);

		$query->page(5);

		$expected = 5;
		$result = $query->page();
		$this->assertEqual($expected, $result);
	}

	public function testOrder() {
		$query = new Query($this->_queryArr);

		$expected = 'created DESC';
		$result = $query->order();
		$this->assertEqual($expected, $result);

		$query->order('updated ASC');

		$expected = 'updated ASC';
		$result = $query->order();
		$this->assertEqual($expected, $result);
	}

	public function testRecord() {
		$query = new Query($this->_queryArr);

		$result = $query->entity();
		$this->assertNull($result);

		$record = (object) array('id' => 12);
		$record->title = 'Lorem Ipsum';

		$query->entity($record);
		$queryRecord = $query->entity();

		$expected = 12;
		$result = $queryRecord->id;
		$this->assertEqual($expected, $result);

		$expected = 'Lorem Ipsum';
		$result = $queryRecord->title;
		$this->assertEqual($expected, $result);

		$this->assertTrue($record === $query->entity());
	}

	public function testComment() {
		$query = new Query($this->_queryArr);

		$expected = 'Find all posts by author 12';
		$result = $query->comment();
		$this->assertEqual($expected, $result);

		$query->comment('Comment lorem');

		$expected = 'Comment lorem';
		$result = $query->comment();
		$this->assertEqual($expected, $result);
	}

	public function testData() {
		$query = new Query($this->_queryArr);

		$expected = array();
		$result = $query->data();
		$this->assertEqual($expected, $result);

		$record = new Record();
		$record->id = 12;
		$record->title = 'Lorem Ipsum';

		$query->entity($record);

		$expected = array('id' => 12, 'title' => 'Lorem Ipsum');
		$result = $query->data();
		$this->assertEqual($expected, $result);

		$query->data(array('id' => 35, 'title' => 'Nix', 'body' => 'Prix'));

		$expected = array('id' => 35, 'title' => 'Nix', 'body' => 'Prix');
		$result = $query->data();
		$this->assertEqual($expected, $result);
	}

	public function testConditions() {
		$query = new Query($this->_queryArr);

		$expected = array('author_id' => 12);
		$result = $query->conditions();
		$this->assertEqual($expected, $result);

		$query->conditions(array('author_id' => 13, 'title LIKE' => 'Lorem%'));

		$expected = array('author_id' => 13, 'title LIKE' => 'Lorem%');
		$result = $query->conditions();
		$this->assertEqual($expected, $result);
	}

	public function testHaving() {
		$query = new Query($this->_queryArr);

		$expected = array();
		$result = $query->having();
		$this->assertEqual($expected, $result);

		$query->having(array('count' => 5));

		$expected = array('count' => 5);
		$result = $query->having();
		$this->assertEqual($expected, $result);
	}

	public function testConditionFromRecord() {
		$entity = new Record();
		$entity->id = 12;
		$query = new Query(compact('entity') + array('model' => $this->_model));

		$expected = array('id' => 12);
		$result = $query->conditions();
		$this->assertEqual($expected, $result);
	}

	public function testExtra() {
		$object = new MockPostObject(array('id' => 1, 'data' => 'test'));
		$query = new Query(array(
			'conditions' => 'foo', 'extra' => 'value', 'extraObject' => $object
		));
		$this->assertEqual(array('foo'), $query->conditions());
		$this->assertEqual('value', $query->extra());
		$this->assertEqual($object, $query->extraObject());
		$this->assertNull($query->extra2());
	}

	public function testExport() {
		MockQueryPost::meta('source', 'foo');
		$query = new Query($this->_queryArr);
		$ds = new MockDatabase();
		$export = $query->export($ds);

		$this->assertTrue(is_array($export));
		$this->skipIf(!is_array($export), 'Query::export() does not return an array');

		$expected = array(
			'alias',
			'calculate',
			'comment',
			'conditions',
			'having',
			'data',
			'fields',
			'group',
			'joins',
			'limit',
			'map',
			'model',
			'offset',
			'order',
			'page',
			'schema',
			'source',
			'type',
			'whitelist',
			'with',
			'relationships'
		);
		$result = array_keys($export);

		sort($expected);
		sort($result);
		$this->assertEqual($expected, $result);

		$expected = '{MockQueryPost}.{id}, {MockQueryPost}.{author_id}, {MockQueryPost}.{title}';
		$result = $export['fields'];
		$this->assertEqual($expected, $result);

		$result = $export['source'];
		$this->assertEqual("{foo}", $result);
	}

	public function testRestrictedKeyExport() {
		$options = array(
			'type' => 'update',
			'data' => array('title' => 'Bar'),
			'conditions' => array('title' => 'Foo'),
			'model' => $this->_model
		);
		$query = new Query($options);

		$result = $query->export($this->db, array(
			'keys' => array('data', 'conditions')
		));
		$expected = array(
			'type' => 'update',
			'data' => array('title' => 'Bar'),
			'conditions' => "WHERE {title} = 'Foo'"
		);
		$this->assertEqual($expected, $result);
	}

	public function testPagination() {
		$query = new Query(array('limit' => 5, 'page' => 1));
		$this->assertEqual(0, $query->offset());

		$query = new Query(array('limit' => 5, 'page' => 2));
		$this->assertEqual(5, $query->offset());

		$query->page(1);
		$this->assertEqual(0, $query->offset());
	}

	public function testJoins() {
		$query = new Query(array('joins' => array(array('foo' => 'bar'))));
		$query->joins(array('bar' => 'baz'));
		$expected = array(array('foo' => 'bar'), array('bar' => 'baz'));
		$joins = $query->joins();
		$this->assertEqual($expected, $joins);

		$this->assertEqual('bar', $joins[0]['foo']);
		$this->assertFalse(isset($joins[0]['bar']));

		$this->assertEqual('baz', $joins[1]['bar']);
		$this->assertFalse(isset($joins[1]['foo']));

		$query->joins('zim', array('dib' => 'gir'));
		$joins = $query->joins();
		$this->assertEqual(3, count($joins));

		$this->assertEqual('gir', $joins['zim']['dib']);

		$expected = array(
			array('foo' => 'bar'),
			array('bar' => 'baz'),
			'zim' => array('dib' => 'gir')
		);
		$this->assertEqual($expected, $joins);
	}

	public function testWithAssociation() {
		$model = $this->_model;
		$model::meta('source', 'foo');
		$model::bind('hasMany', 'MockQueryComment');

		$query = new Query(compact('model') + array('with' => 'MockQueryComment'));
		$export = $query->export(new MockDatabase());

		$expected = array('MockQueryComment' => array(
			'type' => 'hasMany',
			'model' => 'lithium\tests\mocks\data\model\MockQueryComment',
			'fieldName' => 'mock_query_comments',
			'alias' => 'MockQueryComment'
		));
		$keyExists = isset($export['relationships']);
		$this->assertTrue($keyExists);
		$this->skipIf(!$keyExists);
		$this->assertEqual($expected, $export['relationships']);

		$query = new Query(compact('model') + array(
			'type' => 'read',
			'with' => 'MockQueryComment',
			'limit' => 3,
			'order' => 'author_id ASC',
			'group' => 'author_id'
		));
		$expected = 'SELECT * FROM {foo} AS {MockQueryPost} LEFT JOIN AS ';
		$expected .= '{MockQueryComment} ON {MockQueryPost}.{id} = {MockQueryComment}';
		$expected .= '.{mock_query_post_id} GROUP BY author_id ORDER BY author_id ASC ';
		$expected .= 'LIMIT 3;';
		$this->assertEqual($expected, $this->db->renderCommand($query));
	}

	/**
	 * Tests that assigning a whitelist to a query properly restricts the list of data fields that
	 * the query exposes.
	 *
	 * @return void
	 */
	public function testWhitelisting() {
		$data = array('foo' => 1, 'bar' => 2, 'baz' => 3);
		$query = new Query(compact('data'));
		$this->assertEqual($data, $query->data());

		$query = new Query(compact('data') + array('whitelist' => array('foo', 'bar')));
		$this->assertEqual(array('foo' => 1, 'bar' => 2), $query->data());
	}

	/**
	 * Tests basic property accessors and mutators.
	 *
	 * @return void
	 */
	public function testBasicAssignments() {
		$query = new Query();
		$group = array('key' => 'hits', 'reduce' => 'function() {}');
		$calculate = 'count';

		$this->assertNull($query->group());
		$query->group($group);
		$this->assertEqual($group, $query->group());

		$this->assertNull($query->calculate());
		$query->calculate($calculate);
		$this->assertEqual($calculate, $query->calculate());

		$query = new Query(compact('calculate', 'group'));
		$this->assertEqual($group, $query->group());
		$this->assertEqual($calculate, $query->calculate());

		$query->group(false);
		$this->assertNull($query->group());
	}

	public function testInstantiationWithConditionsAndData() {
		$options = array(
			'type' => 'update',
			'data' => array('title' => '..'),
			'conditions' => array('title' => 'FML'),
			'model' => 'lithium\tests\mocks\data\model\MockQueryPost'
		);
		$query = new Query($options);
		$result = $query->export($this->db);

		$this->assertEqual(array('title' => '..'), $result['data']);
		$this->assertEqual("WHERE {title} = 'FML'", $result['conditions']);
	}

	public function testEntityConditions() {
		$entity = new Record(array('model' => $this->_model, 'exists' => true));
		$entity->id = 13;
		$query = new Query(compact('entity'));
		$this->assertEqual(array('id' => 13), $query->conditions());
	}

	public function testInvalidEntityCondition() {
		$entity = new Record(array('model' => $this->_model, 'exists' => true));
		$entity->_id = 13;
		$query = new Query(compact('entity'));
		$this->expectException('/No matching primary key found/');
		$query->conditions();
	}

	public function testAutomaticAliasing() {
		$query = new Query(array('model' => $this->_model));
		$this->assertEqual('MockQueryPost', $query->alias());
	}

	public function testFluentInterface() {
		$query = new Query();
		$conditions = array('foo' => 'bar');
		$fields = array('foo', 'bar', 'baz', 'created');
		$order = array('created' => 'ASC');

		$result = $query->conditions($conditions)->fields($fields)->order($order);
		$this->assertEqual($result, $query);
		$this->assertEqual($conditions, $query->conditions());
		$this->assertEqual($fields, $query->fields());
		$this->assertEqual($order, $query->order());
	}

	/**
	 * The `Query` object shouldn't overwrite custom values with model-supplied values.
	 */
	public function testQueryWithCustomAlias() {
		$model = 'lithium\tests\mocks\data\model\MockQueryComment';

		$query = new Query(compact('model') + array(
			'source' => 'my_custom_table',
			'alias' => 'MyCustomAlias'
		));
		$result = $query->export($this->db);
		$this->assertEqual('{my_custom_table}', $result['source']);
		$this->assertEqual('AS {MyCustomAlias}', $result['alias']);
	}

	public function testRelationships() {
		$query = new Query(array('relationships' => array('Model1' => array('foo' => 'bar'))));
		$query->relationships('Model1.Model2', array('bar' => 'baz'));
		$expected = array(
			'Model1' => array('foo' => 'bar'),
			'Model1.Model2' => array('bar' => 'baz')
		);
		$relationships = $query->relationships();
		$this->assertEqual($expected, $relationships);

		$query = new Query();
		$query->relationships('Model1', array('foo' => 'bar'));
		$query->relationships('Model1.Model2', array('bar' => 'baz'));
		$relationships = $query->relationships();
		$this->assertEqual($expected, $relationships);
	}

	public function testAliasAndPaths() {
		$model = 'lithium\tests\mocks\data\model\MockQueryComment';
		$query = new Query(compact('model'));

		$this->assertIdentical('MockQueryComment', $query->alias());
		$this->assertIdentical('MockQueryComment', $query->alias(true));
		$this->assertIdentical('MockQueryComment2', $query->alias('MockQueryComment2'));
		$this->assertIdentical('MockQueryComment2', $query->alias());
		$this->assertIdentical('MockQueryComment2', $query->alias(true));

		$result = $query->alias('MockQueryComment', 'Model1');
		$this->assertIdentical('MockQueryComment__2', $result);
		$result = $query->alias('MockQueryComment2', 'Model2');
		$this->assertIdentical('MockQueryComment2__2', $result);

		$this->assertIdentical('MockQueryComment__2', $query->alias(true, 'Model1'));
		$this->assertIdentical('MockQueryComment2__2', $query->alias(true, 'Model2'));

		$query = new Query(compact('model') + array(
			'source' => 'my_custom_table',
			'alias' => 'MyCustomAlias'
		));
		$result = $query->export($this->db);
		$this->assertIdentical('{my_custom_table}', $result['source']);
		$this->assertIdentical('AS {MyCustomAlias}', $result['alias']);

		$result = $query->alias('MyCustomAlias', 'Relation1');
		$this->assertIdentical('MyCustomAlias__2', $result);
		$result = $query->alias('MyCustomAlias', 'Other.Relation2');
		$this->assertIdentical('MyCustomAlias__3', $result);
		$result = $query->alias('MyCustomAlias2', 'Other.Other.Relation3');
		$this->assertIdentical('MyCustomAlias2', $result);

		$this->assertIdentical('MyCustomAlias', $query->alias());
		$this->assertIdentical('MyCustomAlias__2', $query->alias(true, 'Relation1'));
		$this->assertIdentical('MyCustomAlias__3', $query->alias(true, 'Other.Relation2'));
		$this->assertIdentical('MyCustomAlias2', $query->alias(true, 'Other.Other.Relation3'));

		$this->assertIdentical('Relation4', $query->alias(null, 'Relation4'));
		$this->assertIdentical('Relation5', $query->alias(null, 'Other.Relation5'));
		$this->assertIdentical('Relation5__2', $query->alias(null, 'Other.Other.Relation5'));

		$this->assertIdentical('Relation4', $query->alias(true, 'Relation4'));
		$this->assertIdentical('Relation5', $query->alias(true, 'Other.Relation5'));
		$this->assertIdentical('Relation5__2', $query->alias(true, 'Other.Other.Relation5'));

		$expected = array (
			'MyCustomAlias' => null,
			'MyCustomAlias__2' => 'Relation1',
			'MyCustomAlias__3' => 'Other.Relation2',
			'MyCustomAlias2' => 'Other.Other.Relation3',
			'Relation4' => 'Relation4',
			'Relation5' => 'Other.Relation5',
			'Relation5__2' => 'Other.Other.Relation5'
		);

		$this->assertEqual($expected, $query->paths($this->db));

		$model = 'lithium\tests\mocks\data\model\MockQueryPost';
		$query = new Query(compact('model'));
		$query->alias(null, 'MockQueryComment');
		$query->alias('MockQueryPost2', 'MockQueryComment.MockQueryPost');

		$expected = array(
			'MockQueryPost' => null,
			'MockQueryComment' => 'MockQueryComment',
			'MockQueryPost2' => 'MockQueryComment.MockQueryPost'
		);
		$this->assertEqual($expected, $query->paths($this->db));
	}

	public function testModels() {
		$model = 'lithium\tests\mocks\data\model\MockQueryPost';
		$query = new Query(array(
			'model' => $model,
			'with' => 'MockQueryComment'
		));

		$expected = array(
			'MockQueryPost' => 'lithium\tests\mocks\data\model\MockQueryPost',
			'MockQueryComment' => 'lithium\tests\mocks\data\model\MockQueryComment'
		);
		$this->assertEqual($expected, $query->models($this->db));

		$query = new Query(array(
			'model' => $model,
			'alias' => 'Post',
			'with' => array(
				'MockQueryComment' => array('alias' => 'Comment'),
				'MockQueryComment.MockQueryPost' => array('alias' => 'Post2'),
			)
		));

		$expected = array(
			'Post' => 'lithium\tests\mocks\data\model\MockQueryPost',
			'Comment' => 'lithium\tests\mocks\data\model\MockQueryComment',
			'Post2' => 'lithium\tests\mocks\data\model\MockQueryPost'
		);
		$this->assertEqual($expected, $query->models($this->db));
	}

	public function testExportWithJoinedStrategy() {
		$query = new Query(array(
			'alias' => 'MyAlias',
			'model' => 'lithium\tests\mocks\data\model\MockGallery',
			'calculate' => 'MyCalculate',
			'comment' => 'No comment',
			'conditions' => array('id' => 2),
			'fields' => array('Tag'),
			'type' => 'read',
			'with' => array('Image.ImageTag.Tag', 'Image', 'Image.ImageTag')
		));
		$export = $query->export($this->db);

		$joins = 'LEFT JOIN {mock_image} AS {Image} ON {MyAlias}.{id} = {Image}.{gallery_id} ';
		$joins .= 'LEFT JOIN {mock_image_tag} AS {ImageTag} ON {Image}.{id} = ';
		$joins .= '{ImageTag}.{image_id} LEFT JOIN {mock_tag} AS {Tag} ON {ImageTag}.{tag_id} = ';
		$joins .= '{Tag}.{id}';

		$expected = array(
			'type' => 'read',
			'alias' => 'AS {MyAlias}',
			'comment' => '/* No comment */',
			'conditions' => 'WHERE {MyAlias}.{id} = 2',
			'fields' => '{MyAlias}.{id}, {Tag}.*, {Image}.{id}, {ImageTag}.{id}',
			'having' => '',
			'group' => null,
			'order' => null,
			'limit' => null,
			'joins' => $joins,
			'model' => 'lithium\tests\mocks\data\model\MockGallery',
			'calculate' => 'MyCalculate',
			'with' => array(
				'Image.ImageTag.Tag' => null,
				'Image' => null,
				'Image.ImageTag' => null
			),
			'source' => '{mock_gallery}',
			'offset' => null,
			'page' => null,
			'data' => array(),
			'whitelist' => array(),
			'schema' => null,
			'map' => array(),
			'relationships' => array(
				'Image' => array(
					'type' => 'hasMany',
					'model' => 'lithium\tests\mocks\data\model\MockImage',
					'fieldName' => 'images',
					'alias' => 'Image'
				),
				'Image.ImageTag' => array(
					'type' => 'hasMany',
					'model' => 'lithium\tests\mocks\data\model\MockImageTag',
					'fieldName' => 'image_tags',
					'alias' => 'ImageTag'
				),
				'Image.ImageTag.Tag' => array(
					'type' => 'belongsTo',
					'model' => 'lithium\tests\mocks\data\model\MockTag',
					'fieldName' => 'tag',
					'alias' => 'Tag'
				)
			)
		);

		$this->assertEqual($expected, $export);
	}

	public function testExportWithUndefinedStrategy() {
		$query = new Query(array(
			'alias' => 'MyAlias',
			'model' => 'lithium\tests\mocks\data\model\MockGallery',
			'calculate' => 'MyCalculate',
			'comment' => 'No comment',
			'conditions' => array('id' => 2),
			'fields' => array('Image.ImageTag.Tag'),
			'type' => 'read',
			'with' => array('Image.ImageTag.Tag', 'Image', 'Image.ImageTag'),
			'strategy' => 'custom'
		));
		$this->expectException('Undefined query strategy `custom`.');
		$export = $query->export($this->db);
	}

	public function testRespondsTo() {
		$query = new Query();
		$this->assertTrue($query->respondsTo('calculate'));
		$this->assertFalse($query->respondsTo('foobarbaz'));
	}

}

?>