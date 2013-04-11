<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\cases\template\helper;

use Exception;
use lithium\action\Request;
use lithium\net\http\Router;
use lithium\data\entity\Record;
use lithium\data\entity\Document;
use lithium\template\helper\Form;
use lithium\tests\mocks\template\helper\MockFormPost;
use lithium\tests\mocks\template\helper\MockFormRenderer;

class FormTest extends \lithium\test\Unit {

	protected $_model = 'lithium\tests\mocks\template\helper\MockFormPost';
	protected $_model2 = 'lithium\tests\mocks\template\helper\MockFormPostInfo';

	/**
	 * Test object instance.
	 *
	 * @var object
	 */
	public $form = null;

	/**
	 * The rendering context object.
	 *
	 * @var object
	 */
	public $context = null;

	public $base = null;

	protected $_routes = array();

	/**
	 * Initialize test by creating a new object instance with a default context.
	 */
	public function setUp() {
		$this->_routes = Router::get();
		Router::reset();
		Router::connect('/{:controller}/{:action}/{:id}.{:type}', array('id' => null));
		Router::connect('/{:controller}/{:action}/{:args}');

		$request = new Request();
		$request->params = array('controller' => 'posts', 'action' => 'index');
		$request->persist = array('controller');

		$this->context = new MockFormRenderer(compact('request'));
		$this->form = new Form(array('context' => $this->context));

		$this->base = $this->context->request()->env('base') . '/';
	}

	public function tearDown() {
		Router::reset();

		foreach ($this->_routes as $route) {
			Router::connect($route);
		}
	}

	public function testFormCreation() {
		$result = $this->form->create();
		$this->assertTags($result, array('form' => array(
			'action' => "{$this->base}posts", 'method' => 'post'
		)));

		$result = $this->form->create(null, array('method' => 'get'));
		$this->assertTags($result, array(
			'form' => array('action' => "{$this->base}posts", 'method' => 'get')
		));

		$result = $this->form->create(null, array('type' => 'file'));
		$this->assertTags($result, array('form' => array(
			'action' => "{$this->base}posts",
			'enctype' => 'multipart/form-data',
			'method' => 'post'
		)));

		$result = $this->form->create(null, array('method' => 'get', 'type' => 'file'));
		$this->assertTags($result, array('form' => array(
			'action' => "{$this->base}posts",
			'method' => 'post',
			'enctype' => 'multipart/form-data'
		)));

		$result = $this->form->create(null, array('id' => 'Registration'));
		$this->assertTags($result, array(
			'form' => array(
				'action' => "{$this->base}posts",
				'method' => 'post',
				'id' => 'Registration'
			)
		));
	}

	/**
	 * Tests creating forms with non-browser compatible HTTP methods, required for REST interfaces.
	 */
	public function testRestFormCreation() {
		$result = $this->form->create(null, array('action' => 'delete', 'method' => 'delete'));

		$this->assertTags($result, array(
			'form' => array(
				'action' => "{$this->base}posts/delete", 'method' => 'post'
			),
			'input' => array('type' => "hidden", 'name' => '_method', 'value' => 'DELETE')
		));

		$result = $this->form->create(null, array('method' => 'put', 'type' => 'file'));
		$this->assertTags($result, array(
			'form' => array(
				'action' => "{$this->base}posts",
				'method' => 'post',
				'enctype' => 'multipart/form-data'
			),
			'input' => array('type' => "hidden", 'name' => '_method', 'value' => 'PUT')
		));

		$record = new Record(array('exists' => true, 'model' => $this->_model));
		$result = $this->form->create($record);

		$this->assertTags($result, array(
			'form' => array('action' => "{$this->base}posts", 'method' => 'post'),
			'input' => array('type' => "hidden", 'name' => '_method', 'value' => 'PUT')
		));
	}

	public function testFormCreationWithBinding() {
		$record = new Record(array('model' => $this->_model, 'data' => array(
			'id' => '5',
			'author_id' => '2',
			'title' => 'This is a saved post',
			'body' => 'This is the body of the saved post'
		)));

		$this->assertTags($this->form->create($record), array(
			'form' => array('action' => "{$this->base}posts", 'method' => 'post')
		));
	}

	/**
	 * Ensures that password fields aren't rendered with pre-populated values from bound record or
	 * document objects.
	 */
	public function testPasswordWithBindingValue() {
		$this->form->create(new Record(array(
			'model' => $this->_model, 'data' => array('pass' => 'foobar')
		)));
		$result = $this->form->password('pass');

		$this->assertTags($result, array(
			'input' => array('type' => 'password', 'name' => 'pass', 'id' => 'MockFormPostPass')
		));
	}

	public function testFormDataBinding() {
		try {
			MockFormPost::config(array('meta' => array('connection' => false)));
		} catch (Exception $e) {
			MockFormPost::config(array('meta' => array('connection' => false)));
		}

		$record = new Record(array('model' => $this->_model, 'data' => array(
			'id' => '5',
			'author_id' => '2',
			'title' => 'This is a saved post',
			'body' => 'This is the body of the saved post',
			'zeroInt' => 0,
			'zeroString' => "0"
		)));

		$result = $this->form->create($record);
		$this->assertTags($result, array(
			'form' => array('action' => "{$this->base}posts", 'method' => 'post')
		));

		$result = $this->form->text('title');
		$this->assertTags($result, array('input' => array(
			'type' => 'text', 'name' => 'title',
			'value' => 'This is a saved post', 'id' => 'MockFormPostTitle'
		)));


		$result = $this->form->text('zeroInt');
		$this->assertTags($result, array('input' => array(
			'type' => 'text', 'name' => 'zeroInt',
			'value' => '0', 'id' => 'MockFormPostZeroInt'
		)));
		$result = $this->form->text('zeroString');
		$this->assertTags($result, array('input' => array(
			'type' => 'text', 'name' => 'zeroString',
			'value' => '0', 'id' => 'MockFormPostZeroString'
		)));

		$this->assertEqual('</form>', $this->form->end());

		$this->assertTags($this->form->text('title'), array('input' => array(
			'type' => 'text', 'name' => 'title', 'id' => 'Title'
		)));
	}

	public function testTextBox() {
		$result = $this->form->text('foo');
		$this->assertTags($result, array('input' => array(
			'type' => 'text', 'name' => 'foo', 'id' => 'Foo'
		)));
	}

	public function testElementsWithDefaultConfiguration() {
		$this->form = new Form(array(
			'context' => new MockFormRenderer(), 'base' => array('class' => 'editable')
		));

		$result = $this->form->text('foo');
		$this->assertTags($result, array('input' => array(
			'type' => 'text', 'name' => 'foo', 'class' => 'editable', 'id' => 'Foo'
		)));

		$this->form->config(array('base' => array('maxlength' => 255)));

		$result = $this->form->text('foo');
		$this->assertTags($result, array('input' => array(
			'type' => 'text', 'name' => 'foo', 'class' => 'editable',
			'maxlength' => '255', 'id' => 'Foo'
		)));

		$this->form->config(array('text' => array('class' => 'locked')));

		$result = $this->form->text('foo');
		$this->assertTags($result, array('input' => array(
			'type' => 'text', 'name' => 'foo', 'class' => 'locked',
			'maxlength' => '255', 'id' => 'Foo'
		)));

		$result = $this->form->config();
		$expected = array(
			'base' => array('class' => 'editable', 'maxlength' => 255),
			'text' => array('class' => 'locked'),
			'textarea' => array(),
			'templates' => array('create' => 'form', 'end' => 'form-end'),
			'attributes' => array(
				'id' => $result['attributes']['id'],
				'name' => $result['attributes']['name']
			)
		);
		$this->assertEqual($expected, $result);
		$this->assertTrue(is_callable($result['attributes']['id']));
		$this->assertTrue(is_callable($result['attributes']['name']));
	}

	public function testFormElementWithDefaultValue() {
		$result = $this->form->text('foo', array('default' => 'Message here'));

		$this->assertTags($result, array('input' => array(
			'type' => 'text', 'name' => 'foo', 'value' => 'Message here', 'id' => 'Foo'
		)));

		$result = $this->form->text('foo', array(
			'default' => 'Message here', 'value' => 'My Name Is Jonas', 'id' => 'Foo'
		));
		$this->assertTags($result, array('input' => array(
			'type' => 'text', 'name' => 'foo', 'value' => 'My Name Is Jonas', 'id' => 'Foo'
		)));

		$result = $this->form->text('foo', array('value' => 'My Name Is Jonas'));
		$this->assertTags($result, array('input' => array(
			'type' => 'text', 'name' => 'foo', 'value' => 'My Name Is Jonas', 'id' => 'Foo'
		)));
	}

	public function testFormInputField() {
		$tag = array('input' => array('type' => 'file', 'name' => 'upload', 'id' => 'Upload'));

		$result = $this->form->file('upload');
		$this->assertTags($result, $tag);

		$value = new Document(array('model' => $this->_model));
		$result = $this->form->file('upload', compact('value'));
		$tag['input']['value'] = '';
		$this->assertTags($result, $tag);
	}

	public function testHiddenFieldWithId() {
		$result = $this->form->hidden('my_field');
		$this->assertTags($result, array('input' => array(
			'type' => 'hidden', 'name' => 'my_field', 'id' => 'MyField'
		)));
	}

	public function testLabelGeneration() {
		$result = $this->form->label('next', 'Enter the next value >>');
		$this->assertTags($result, array(
			'label' => array('for' => 'next'),
			'Enter the next value &gt;&gt;',
			'/label'
		));

		$result = $this->form->label('user_name');
		$this->assertTags($result, array(
			'label' => array('for' => 'user_name'),
			'User Name',
			'/label'
		));

		$result = $this->form->label('first_name', array(
			'First Name' => array('id' => 'first_name_label')
		));
		$this->assertTags($result, array(
			'label' => array('for' => 'first_name', 'id' => 'first_name_label'),
			'First Name',
			'/label'
		));

		$result = $this->form->label('first_name', array(
			null => array('id' => 'first_name_label')
		));
		$this->assertTags($result, array(
			'label' => array('for' => 'first_name', 'id' => 'first_name_label'),
			'First Name',
			'/label'
		));
	}

	public function testLabelGenerationWithNoEscape() {
		$result = $this->form->label('next', 'Enter the next value >>', array('escape' => false));
		$this->assertTags($result, array(
			'label' => array('for' => 'next'),
			'Enter the next value >>',
			'/label'
		));
	}

	public function testSubmitGeneration() {
		$result = $this->form->submit('Continue >');
		$this->assertTags($result, array('input' => array(
			'type' => 'submit',
			'value' => 'Continue &gt;'
		)));

		$result = $this->form->submit('Continue >', array('class' => 'special'));
		$this->assertTags($result, array('input' => array(
			'type' => 'submit',
			'value' => 'Continue &gt;',
			'class' => 'special'
		)));
	}

	public function testTextareaGeneration() {
		$result = $this->form->textarea('foo', array('value' => 'some content >'));
		$this->assertTags($result, array(
			'textarea' => array('name' => 'foo', 'id' => 'Foo'),
			'some content &gt;',
			'/textarea'
		));
	}

	public function testCheckboxGeneration() {
		$result = $this->form->checkbox('foo');
		$this->assertTags($result, array(
			array('input' => array('type' => 'hidden', 'value' => '', 'name' => 'foo')),
			array('input' => array(
				'type' => 'checkbox', 'value' => '1', 'name' => 'foo', 'id' => 'Foo'
			))
		));

		$result = $this->form->checkbox('foo', array('checked' => false));
		$this->assertTags($result, array(
			array('input' => array('type' => 'hidden', 'value' => '', 'name' => 'foo')),
			array('input' => array(
				'type' => 'checkbox', 'value' => '1', 'name' => 'foo', 'id' => 'Foo'
			))
		));

		$result = $this->form->checkbox('foo', array('checked' => true));
		$this->assertTags($result, array(
			array('input' => array('type' => 'hidden', 'value' => '', 'name' => 'foo')),
			array('input' => array(
				'type' => 'checkbox', 'value' => '1', 'name' => 'foo',
				'checked' => 'checked', 'id' => 'Foo'
			))
		));

		$record = new Record(array('model' => $this->_model, 'data' => array('foo' => true)));
		$this->form->create($record);

		$result = $this->form->checkbox('foo');
		$this->assertTags($result, array(
			array('input' => array('type' => 'hidden', 'value' => '', 'name' => 'foo')),
			array('input' => array(
				'type' => 'checkbox', 'value' => '1', 'name' => 'foo', 'id' => 'MockFormPostFoo',
				'checked' => 'checked'
			))
		));

		$record = new Record(array('model' => $this->_model, 'data' => array('foo' => false)));
		$this->form->create($record);

		$result = $this->form->checkbox('foo');
		$this->assertTags($result, array(
			array('input' => array('type' => 'hidden', 'value' => '', 'name' => 'foo')),
			array('input' => array(
				'type' => 'checkbox', 'value' => '1', 'name' => 'foo', 'id' => 'MockFormPostFoo'
			))
		));

		$document = new Document(array(
			'model' => $this->_model,
			'data' => array(
				'subdocument' => array(
					'foo' => true
				)
			)
		));
		$this->form->create($document);

		$result = $this->form->checkbox('subdocument.foo');
		$this->assertTags($result, array(
			array('input' => array(
				'type' => 'hidden', 'value' => '', 'name' => 'subdocument[foo]')
			),
			array('input' => array(
				'type' => 'checkbox', 'value' => '1', 'name' => 'subdocument[foo]',
				'checked' => 'checked', 'id' => 'MockFormPostSubdocumentFoo'
			))
		));
	}

	public function testCustomCheckbox() {
		$result = $this->form->checkbox('foo', array('value' => '1'));
		$this->assertTags($result, array(
			array('input' => array('type' => 'hidden', 'value' => '', 'name' => 'foo')),
			array('input' => array(
				'type' => 'checkbox', 'value' => '1',  'name' => 'foo', 'id' => 'Foo'
			))
		));

		$result = $this->form->checkbox('foo', array('checked' => true, 'value' => '1'));
		$this->assertTags($result, array(
			array('input' => array('type' => 'hidden', 'value' => '', 'name' => 'foo')),
			array('input' => array(
				'type' => 'checkbox', 'value' => '1',  'name' => 'foo',
				'checked' => 'checked', 'id' => 'Foo'
			))
		));

		$record = new Record(array('model' => $this->_model, 'data' => array('foo' => true)));
		$this->form->create($record);

		$result = $this->form->checkbox('foo', array('value' => '1'));
		$this->assertTags($result, array(
			array('input' => array('type' => 'hidden', 'value' => '', 'name' => 'foo')),
			array('input' => array(
				'type' => 'checkbox', 'value' => '1',  'name' => 'foo',
				'id' => 'MockFormPostFoo', 'checked' => 'checked'
			))
		));

		$result = $this->form->checkbox('foo', array('value' => true));
		$this->assertTags($result, array(
			array('input' => array('type' => 'hidden', 'value' => '', 'name' => 'foo')),
			array('input' => array(
				'type' => 'checkbox', 'value' => '1',  'name' => 'foo',
				'id' => 'MockFormPostFoo', 'checked' => 'checked'
			))
		));
	}

	public function testCustomValueCheckbox() {
		$result = $this->form->checkbox('foo', array('value' => 'HERO'));
		$this->assertTags($result, array(
			array('input' => array('type' => 'hidden', 'value' => '', 'name' => 'foo')),
			array('input' => array(
				'type' => 'checkbox', 'value' => 'HERO', 'name' => 'foo', 'id' => 'Foo'
			))
		));

		$result = $this->form->checkbox('foo', array('value' => 'nose'));
		$this->assertTags($result, array(
			array('input' => array('type' => 'hidden', 'value' => '', 'name' => 'foo')),
			array('input' => array(
				'type' => 'checkbox', 'value' => 'nose', 'name' => 'foo', 'id' => 'Foo'
			))
		));

		$record = new Record(array('model' => $this->_model, 'data' => array('foo' => 'nose')));
		$this->form->create($record);

		$result = $this->form->checkbox('foo', array('value' => 'nose'));
		$this->assertTags($result, array(
			array('input' => array('type' => 'hidden', 'value' => '', 'name' => 'foo')),
			array('input' => array(
				'type' => 'checkbox', 'value' => 'nose', 'name' => 'foo',
				'id' => 'MockFormPostFoo', 'checked' => 'checked'
			))
		));

		$record = new Record(array('model' => $this->_model, 'data' => array('foo' => 'foot')));
		$this->form->create($record);

		$result = $this->form->checkbox('foo', array('value' => 'nose'));
		$this->assertTags($result, array(
			array('input' => array('type' => 'hidden', 'value' => '', 'name' => 'foo')),
			array('input' => array(
				'type' => 'checkbox', 'value' => 'nose', 'name' => 'foo', 'id' => 'MockFormPostFoo'
			))
		));

		$record = new Record(array('model' => $this->_model, 'data' => array('foo' => false)));
		$this->form->create($record);
		$result = $this->form->checkbox('foo', array('value' => '0'));
		$this->assertTags($result, array(
			array('input' => array('type' => 'hidden', 'value' => '', 'name' => 'foo')),
			array('input' => array(
				'type' => 'checkbox', 'value' => '0', 'name' => 'foo', 'id' => 'MockFormPostFoo',
				'checked' => 'checked'
			))
		));
	}

	public function testRadioGeneration() {
		$result = $this->form->radio('foo');
		$this->assertTags($result, array(array(
			'input' => array('type' => 'radio', 'value' => '1', 'name' => 'foo', 'id' => 'Foo'))
		));

		$result = $this->form->radio('foo', array('checked' => false));
		$this->assertTags($result, array(array(
			'input' => array('type' => 'radio', 'value' => '1', 'name' => 'foo', 'id' => 'Foo'))
		));

		$result = $this->form->radio('foo', array('checked' => true));
		$this->assertTags($result, array(array(
			'input' => array(
				'type' => 'radio',
				'value' => '1',
				'name' => 'foo',
				'checked' => 'checked',
				'id' => 'Foo')
			)
		));

		$record = new Record(array('model' => $this->_model, 'data' => array('foo' => true)));
		$this->form->create($record);

		$result = $this->form->radio('foo');
		$this->assertTags($result, array(
			array('input' => array(
				'type' => 'radio', 'value' => '1', 'name' => 'foo', 'id' => 'MockFormPostFoo',
				'checked' => 'checked'
			))
		));

		$record = new Record(array('model' => $this->_model, 'data' => array('foo' => false)));
		$this->form->create($record);

		$result = $this->form->radio('foo');
		$this->assertTags($result, array(
			array('input' => array(
				'type' => 'radio', 'value' => '1', 'name' => 'foo', 'id' => 'MockFormPostFoo'
			))
		));
	}

	public function testCustomRadio() {
		$result = $this->form->radio('foo', array('value' => '1'));
		$this->assertTags($result, array(
			array('input' => array(
				'type' => 'radio', 'value' => '1',  'name' => 'foo', 'id' => 'Foo'
			))
		));

		$result = $this->form->radio('foo', array('checked' => true, 'value' => '1'));
		$this->assertTags($result, array(
			array('input' => array(
				'type' => 'radio', 'value' => '1',  'name' => 'foo',
				'checked' => 'checked', 'id' => 'Foo'
			))
		));

		$record = new Record(array('model' => $this->_model, 'data' => array('foo' => true)));
		$this->form->create($record);

		$result = $this->form->radio('foo', array('value' => '1'));
		$this->assertTags($result, array(
			array('input' => array(
				'type' => 'radio', 'value' => '1',  'name' => 'foo',
				'id' => 'MockFormPostFoo', 'checked' => 'checked'
			))
		));

		$result = $this->form->radio('foo', array('value' => true));
		$this->assertTags($result, array(
			array('input' => array(
				'type' => 'radio', 'value' => '1',  'name' => 'foo',
				'id' => 'MockFormPostFoo', 'checked' => 'checked'
			))
		));
	}

	public function testCustomValueRadio() {
		$result = $this->form->radio('foo', array('value' => 'HERO'));
		$this->assertTags($result, array(
			array('input' => array(
				'type' => 'radio', 'value' => 'HERO', 'name' => 'foo', 'id' => 'Foo'
			))
		));

		$result = $this->form->radio('foo', array('value' => 'nose'));
		$this->assertTags($result, array(
			array('input' => array(
				'type' => 'radio', 'value' => 'nose', 'name' => 'foo', 'id' => 'Foo'
			))
		));

		$record = new Record(array('model' => $this->_model, 'data' => array('foo' => 'nose')));
		$this->form->create($record);

		$result = $this->form->radio('foo', array('value' => 'nose'));
		$this->assertTags($result, array(
			array('input' => array(
				'type' => 'radio', 'value' => 'nose', 'name' => 'foo',
				'id' => 'MockFormPostFoo', 'checked' => 'checked'
			))
		));

		$record = new Record(array('model' => $this->_model, 'data' => array('foo' => 'foot')));
		$this->form->create($record);

		$result = $this->form->checkbox('foo', array('value' => 'nose'));
		$this->assertTags($result, array(
			array('input' => array('type' => 'hidden', 'value' => '', 'name' => 'foo')),
			array('input' => array(
				'type' => 'checkbox', 'value' => 'nose', 'name' => 'foo', 'id' => 'MockFormPostFoo'
			))
		));

		$record = new Record(array('model' => $this->_model, 'data' => array('foo' => false)));
		$this->form->create($record);
		$result = $this->form->radio('foo', array('value' => '0'));
		$this->assertTags($result, array(
			array('input' => array(
				'type' => 'radio', 'value' => '0',  'name' => 'foo',
				'id' => 'MockFormPostFoo', 'checked' => 'checked'
			))
		));
	}

	public function testSelectGeneration() {
		$result = $this->form->select('foo');

		$this->assertTags($result, array(
			'select' => array('name' => 'foo', 'id' => 'Foo'), '/select'
		));

		$result = $this->form->select(
			'colors',
			array('r' => 'red', 'g "' => 'green', 'b' => 'blue'),
			array('id' => 'Colors', 'value' => 'g "')
		);

		$this->assertTags($result, array(
			'select' => array('name' => 'colors', 'id' => 'Colors'),
			array('option' => array('value' => 'r')),
			'red',
			'/option',
			array('option' => array('value' => 'g &quot;', 'selected' => 'selected')),
			'green',
			'/option',
			array('option' => array('value' => 'b')),
			'blue',
			'/option',
			'/select'
		));
	}

	/**
	 * When trying to determine which option of a select box should be selected, we should be
	 * integer/string agnostic because it all looks the same in HTML.
	 *
	 */
	public function testSelectTypeAgnosticism() {
		$taglist = array(
			'select' => array('name' => 'numbers', 'id' => 'Numbers'),
			array('option' => array('value' => '0')),
			'Zero',
			'/option',
			array('option' => array('value' => '1', 'selected' => 'selected')),
			'One',
			'/option',
			array('option' => array('value' => '2')),
			'Two',
			'/option',
			'/select'
		);

		$result = $this->form->select(
			'numbers',
			array(0 => 'Zero', 1 => 'One', 2 => 'Two'),
			array('id' => 'Numbers', 'value' => '1')
		);

		$this->assertTags($result, $taglist);

		$result = $this->form->select(
			'numbers',
			array('0' => 'Zero', '1' => 'One', '2' => 'Two'),
			array('id' => 'Numbers', 'value' => 1)
		);

		$this->assertTags($result, $taglist);
	}

	public function testSelectWithEmptyOption() {
		$result = $this->form->select('numbers', array('zero', 'first', 'second'), array(
			'empty' => true
		));

		$this->assertTags($result, array(
			'select' => array('id' => 'Numbers', 'name' => 'numbers'),
			array('option' => array('value' => '', 'selected' => 'selected')),
			'/option',
			array('option' => array('value' => '0')),
			'zero',
			'/option',
			array('option' => array('value' => '1')),
			'first',
			'/option',
			array('option' => array('value' => '2')),
			'second',
			'/option',
			'/select'
		));

		$result = $this->form->select('numbers', array('zero', 'first', 'second'), array(
			'empty' => '> Make a selection'
		));

		$this->assertTags($result, array(
			'select' => array('name' => 'numbers', 'id' => 'Numbers'),
			array('option' => array('value' => '', 'selected' => 'selected')),
			'&gt; Make a selection',
			'/option',
			array('option' => array('value' => '0')),
			'zero',
			'/option',
			array('option' => array('value' => '1')),
			'first',
			'/option',
			array('option' => array('value' => '2')),
			'second',
			'/option',
			'/select'
		));
	}

	/**
	 * Tests that calling `select()` with nested arrays will produce lists of `<option />`s wrapped
	 * in `<optgroup />` elements.
	 */
	public function testRecursiveSelect() {
		$list = array(
			'Linux' => array(
				'1' => 'Ubuntu 10.10',
				'2' => 'CentOS 5'
			),
			'Other' => array(
				'4' => 'Solaris',
				'5' => 'Windows Server 2010 R2'
			)
		);
		$result = $this->form->select('opsys', $list, array(
			'empty' => 'Select one', 'value' => '5'
		));

		$this->assertTags($result, array(
			'select' => array('name' => 'opsys', 'id' => 'Opsys'),
			array('option' => array('value' => '')),
			'Select one',
			'/option',
			array('optgroup' => array('label' => 'Linux')),
			array('option' => array('value' => '1')),
			'Ubuntu 10.10',
			'/option',
			array('option' => array('value' => '2')),
			'CentOS 5',
			'/option',
			'/optgroup',
			array('optgroup' => array('label' => 'Other')),
			array('option' => array('value' => '4')),
			'Solaris',
			'/option',
			array('option' => array('value' => '5', 'selected' => 'selected')),
			'Windows Server 2010 R2',
			'/option',
			'/optgroup',
			'/select'
		));
	}

	public function testTemplateRemapping() {
		$result = $this->form->password('passwd');
		$this->assertTags($result, array('input' => array(
			'type' => 'password', 'name' => 'passwd', 'id' => 'Passwd'
		)));

		$this->form->config(array('templates' => array('password' => 'text')));

		$result = $this->form->password('passwd');
		$this->assertTags($result, array('input' => array(
			'type' => 'text', 'name' => 'passwd', 'id' => 'Passwd'
		)));
	}

	public function testMultiSelect() {
		$expected = array(
			'select' => array('name' => 'numbers[]', 'id' => 'Numbers', 'multiple' => 'multiple'),
			array('option' => array('value' => '', 'selected' => 'selected')),
			'&gt; Make a selection',
			'/option',
			array('option' => array('value' => '1')),
			'first',
			'/option',
			array('option' => array('value' => '2')),
			'second',
			'/option',
			'/select'
		);
		$result = $this->form->select('numbers', array('1' => 'first', '2' => 'second'), array(
			'empty' => '> Make a selection',
			'multiple' => true
		));
		$this->assertTags($result, $expected);

		$expected = array(
			'select' => array(
				'name' => 'numbers[]', 'multiple' => 'multiple', 'size' => 5, 'id' => 'Numbers'
			),
			array('option' => array('value' => '1')),
			'first',
			'/option',
			array('option' => array('value' => '2')),
			'second',
			'/option',
			'/select'
		);
		$result = $this->form->select('numbers', array('1' => 'first', '2' => 'second'), array(
			'multiple' => true,
			'size' => 5
		));
		$this->assertTags($result, $expected);
	}

	public function testMultiselected() {
		$expected = array(
			'select' => array('name' => 'numbers[]', 'id' => 'Numbers', 'multiple' => 'multiple'),
			array('option' => array('value' => '1', 'selected' => 'selected')),
			'first',
			'/option',
			array('option' => array('value' => '2')),
			'second',
			'/option',
			array('option' => array('value' => '3', 'selected' => 'selected')),
			'third',
			'/option',
			array('option' => array('value' => '4', 'selected' => 'selected')),
			'fourth',
			'/option',
			'/select'
		);
		$list = array(1 => 'first', 2 => 'second', 3 => 'third', 4 => 'fourth');
		$options = array('value' => array(1, 3, 4), 'multiple' => true);
		$result = $this->form->select('numbers', $list, $options);
		$this->assertTags($result, $expected);
	}

	public function testFormCreateWithMoreParams() {
		$request = new Request();
		$request->params = array('controller' => 'mock', 'action' => 'test', 'args' => array('1'));
		$context = new MockFormRenderer(compact('request'));
		$form = new Form(compact('context'));

		$result = $form->create();
		$this->assertTags($result, array('form' => array(
			'action' => "{$this->base}mock/test/1",
			'method' => 'post'
		)));
	}

	public function testFormCreateWithMoreParamsButSpecifiedAction() {
		$request = new Request();
		$request->params = array('controller' => 'mock', 'action' => 'test', 'args' => array('1'));
		$request->persist = array('controller');
		$context = new MockFormRenderer(compact('request'));
		$form = new Form(compact('context'));

		$result = $form->create(null, array('action' => 'radness'));
		$this->assertTags($result, array('form' => array(
			'action' => "{$this->base}mock/radness",
			'method' => 'post'
		)));
	}

	public function testFormField() {
		$result = $this->form->field('name');
		$this->assertTags($result, array(
			'div' => array(),
			'label' => array('for' => 'Name'), 'Name', '/label',
			'input' => array('type' => 'text', 'name' => 'name', 'id' => 'Name'),
			'/div'
		));

		$result = $this->form->field('name', array('type' => 'radio', 'value' => 'foo'));
		$this->assertTags($result, array(
			'div' => array(),
			'input' => array('type' => 'radio', 'name' => 'name', 'value' => 'foo', 'id' => 'Name'),
			'label' => array('for' => 'Name'), 'Name', '/label',
			'/div'
		));

		$result = $this->form->field('name', array('type' => 'checkbox'));
		$expected = array(
			'<div>',
			'<input type="hidden" name="name" value="" />',
			'<input type="checkbox" name="name" id="Name" value="1" />',
			'<label for="Name">Name</label></div>'
		);
		$this->assertEqual(join('', $expected), $result);
	}

	public function testFormFieldWithCustomConfig() {
		$this->form->config(array('field' => array('class' => 'custom-field')));
		$result = $this->form->field('username');

		$this->assertTags($result, array(
			'div' => array(),
			'label' => array('for' => 'Username'),
			'Username',
			'/label',
			'input' => array(
				'type' => 'text',
				'name' => 'username',
				'class' => 'custom-field',
				'id' => 'Username'
			),
			'/div'
		));

		$this->assertTags($this->form->end(), array('/form'));

		$this->form->config(array('templates' => array('end' => "</table></form>")));
		$this->assertTags($this->form->end(), array('/table', '/form'));
	}

	/**
	 * Verifies that calls to `field()` with `'type' => 'hidden'` do not produce `<label />`s.
	 */
	public function testHiddenFieldWithNoLabel() {
		$result = $this->form->field('foo', array('type' => 'hidden'));
		$this->assertTags($result, array(
			'div' => array(),
			'input' => array('type' => 'hidden', 'name' => 'foo', 'id' => 'Foo'),
			'/div'
		));
	}

	public function testFormFieldWithCustomTemplate() {
		$result = $this->form->field('name', array(
			'template' => '<div{:wrap}>{:label}: {:input}{:error}</div>'
		));
		$this->assertTags($result, array(
			'div' => array(),
			'label' => array('for' => 'Name'), 'Name', '/label', ':',
			'input' => array('type' => 'text', 'name' => 'name', 'id' => 'Name')
		));
	}

	public function testFieldWithLabelShorthand() {
		$result = $this->form->field(array('name' => 'Enter a name'));
		$this->assertTags($result, array(
			'div' => array(),
			'label' => array('for' => 'Name'), 'Enter a name', '/label',
			'input' => array('type' => 'text', 'name' => 'name', 'id' => 'Name')
		));
	}

	/**
	 * Demonstrates that the options for a `<label />` element can be passed through the `field()`
	 * method, using the label text as a key.
	 */
	public function testFieldLabelWithOptions() {
		$result = $this->form->field('name', array(
			'label' => array('Item Name' => array('class' => 'required'))
		));
		$this->assertTags($result, array(
			'div' => array(),
			'label' => array('for' => 'Name', 'class' => 'required'), 'Item Name', '/label',
			'input' => array('type' => 'text', 'name' => 'name', 'id' => 'Name')
		));

		$result = $this->form->field('video_preview', array(
			'label' => array('<a href="http://www.youtube.com/">Youtube</a>' => array(
				'escape' => false
			))
		));
		$this->assertTags($result, array(
			'div' => array(),
			'label' => array('for' => 'VideoPreview'),
			'a' => array('href' => 'http://www.youtube.com/'), 'Youtube', '/a', '/label',
			'input' => array('type' => 'text', 'name' => 'video_preview', 'id' => 'VideoPreview')
		));
	}

	public function testMultipleFields() {
		$result = $this->form->field(array(
			'name' => 'Enter a name',
			'phone_number',
			'email' => 'Enter a valid email'
		));
		$this->assertTags($result, array(
			array('div' => array()),
				array('label' => array('for' => 'Name')),
					'Enter a name',
				'/label',
				array('input' => array('type' => 'text', 'name' => 'name', 'id' => 'Name')),
			'/div',

			array('div' => array()),
				array('label' => array('for' => 'PhoneNumber')),
					'Phone Number',
				'/label',
				array('input' => array(
					'type' => 'text', 'name' => 'phone_number', 'id' => 'PhoneNumber'
				)),
			'/div',

			array('div' => array()),
				array('label' => array('for' => 'Email')),
					'Enter a valid email',
				'/label',
				array('input' => array('type' => 'text', 'name' => 'email', 'id' => 'Email')),
			'/div'
		));
	}

	public function testCustomInputTypes() {
		// Creates an HTML5 'range' input slider:
		$range = $this->form->range('completion', array('min' => 0, 'max' => 100));
		$this->assertTags($range, array('input' => array(
			'type' => 'range', 'name' => 'completion',
			'min' => '0', 'max' => '100', 'id' => 'Completion'
		)));
	}

	public function testFieldWithCustomType() {
		$field = $this->form->field('completion', array(
			'type' => 'range', 'id' => 'completion', 'min' => '0', 'max' => '100',
			'label' => 'Completion %', 'wrap' => array('class' => 'input')
		));
		$this->assertTags($field, array(
			'div' => array('class' => 'input'),
			'label' => array('for' => 'completion'), 'Completion %', '/label',
			'input' => array(
				'type' => 'range', 'name' => 'completion',
				'id' => 'completion', 'min' => '0', 'max' => '100'
			),
			'/div'
		));
	}

	public function testFormFieldSelect() {
		$result = $this->form->field('states', array(
			'type' => 'select', 'list' => array('CA', 'RI')
		));
		$this->assertTags($result, array(
			'div' => array(),
			'label' => array('for' => 'States'), 'States', '/label',
			'select' => array('name' => 'states', 'id' => 'States'),
			array('option' => array('value' => '0')),
			'CA',
			'/option',
			array('option' => array('value' => '1')),
			'RI',
			'/option',
			'/select'
		));
	}

	public function testFormErrorWithout() {
		$this->form->create(null);
		$result = $this->form->error('name');
		$this->assertTrue(is_null($result));
	}

	public function testFormErrorWithRecordAndStringError() {
		$record = new Record(array('model' => $this->_model));
		$record->errors(array('name' => 'Please enter a name'));
		$this->form->create($record);

		$result = $this->form->error('name');
		$this->assertTags($result, array(
			'div' => array('class' => 'error'), 'Please enter a name', '/div'
		));
	}

	public function testFormMultipleErrors() {
		$record = new Record(array('model' => $this->_model));
		$record->errors(array('email' => array('Empty', 'Valid')));
		$this->form->create($record);

		$result = $this->form->error('email');
		$this->assertTags($result, array(
			array('div' => array('class' => 'error')), 'Empty', '/div',
			array('div' => array('class' => 'error')), 'Valid', '/div'
		));

		$result = $this->form->error('email', 0);
		$this->assertTags($result, array('div' => array('class' => 'error'), 'Empty', '/div'));

		$result = $this->form->error('email', 1);
		$this->assertTags($result, array('div' => array('class' => 'error'), 'Valid', '/div'));

		$result = $this->form->error('email', true);
		$this->assertTags($result, array('div' => array('class' => 'error'), 'Empty', '/div'));
	}

	public function testFormErrorWithRecordAndSpecificKey() {
		$record = new Record(array('model' => $this->_model));
		$record->errors(array('name' => array('Please enter a name')));
		$this->form->create($record);

		$result = $this->form->error('name', 0);
		$this->assertTags($result, array(
			'div' => array('class' => 'error'), 'Please enter a name', '/div'
		));
	}

	public function testFormErrorWithRecordAndSpecificKeyAndValue() {
		$record = new Record(array('model' => $this->_model));
		$record->name = 'Nils';
		$record->errors(array('name' => array('Please enter a name')));
		$this->form->create($record);

		$result = $this->form->error('name');
		$this->assertTags($result, array(
			'div' => array('class' => 'error'), 'Please enter a name', '/div'
		));
	}

	public function testFormFieldWithError() {
		$record = new Record(array('model' => $this->_model));
		$record->errors(array('name' => array('Please enter a name')));
		$this->form->create($record);

		$result = $this->form->field('name');
		$this->assertTags($result, array(
			'<div', 'label' => array('for' => 'MockFormPostName'), 'Name', '/label',
			'input' => array('type' => "text", 'name' => 'name', 'id' => 'MockFormPostName'),
			'div' => array('class' => "error"), 'Please enter a name', '/div', '/div'
		));
	}

	public function testErrorWithCustomConfiguration() {
		$this->form->config(array('error' => array('class' => 'custom-error-class')));

		$record = new Record(array('model' => $this->_model));
		$record->errors(array('name' => array('Please enter a name')));
		$this->form->create($record);

		$result = $this->form->field('name');
		$this->assertTags($result, array(
			'<div', 'label' => array('for' => 'MockFormPostName'), 'Name', '/label',
			'input' => array('type' => "text", 'name' => 'name', 'id' => 'MockFormPostName'),
			'div' => array('class' => "custom-error-class"), 'Please enter a name', '/div', '/div'
		));
	}

	/**
	 * Tests that the string template form `Form::field()` can be overridden.
	 */
	public function testFieldTemplateOverride() {
		$this->form->config(array('templates' => array('field' => '{:label}{:input}{:error}')));
		$result = $this->form->field('name', array('type' => 'text'));
		$this->assertTags($result, array(
			'label' => array('for' => 'Name'), 'Name', '/label',
			'input' => array('type' => 'text', 'name' => 'name', 'id' => 'Name')
		));
	}

	/**
	 * Tests that the `field()` method properly renders a `<select />` element if the `'list'`
	 * option is passed.
	 */
	public function testFieldAssumeSelectIfList() {
		$result = $this->form->field('colors', array(
			'list' => array('r' => 'red', 'g' => 'green', 'b' => 'blue')
		));
		$expected = array(
			'<div',
				array('label' => array('for' => 'Colors')),
					'Colors',
				'/label',
				'select' => array('name' => 'colors', 'id' => 'Colors'),
					array('option' => array('value' => 'r')),
						'red',
					'/option',
					array('option' => array('value' => 'g')),
						'green',
					'/option',
					array('option' => array('value' => 'b')),
						'blue',
					'/option',
				'/select',
			'/div'
		);
		$this->assertTags($result, $expected);
	}

	public function testFieldInputIdWithFormId() {
		$this->form->create(null, array('id' => 'registration'));
		$result = $this->form->field('name');

		$this->assertTags($result, array(
			'div' => array(),
			'label' => array('for' => 'Name'), 'Name', '/label',
			'input' => array('type' => 'text', 'name' => 'name', 'id' => 'Name')
		));
	}

	/**
	 * Tests that inputs for nested objects can be assigned using dot syntax.
	 */
	public function testNestedFieldAccess() {
		$doc = new Document(array('data' => array('foo' => array('bar' => 'value'))));
		$this->form->create($doc);

		$result = $this->form->text('foo.bar');
		$this->assertTags($result, array('input' => array(
			'type' => 'text', 'name' => 'foo[bar]', 'id' => 'FooBar', 'value' => 'value'
		)));

		$result = $this->form->field('foo.bar');
		$this->assertTags($result, array(
			'div' => array(),
			'label' => array('for' => 'FooBar'), 'Foo Bar', '/label',
			'input' => array(
				'type' => 'text', 'name' => 'foo[bar]', 'id' => 'FooBar', 'value' => 'value'
			)
		));
	}

	/**
	 * Tests rendering errors for nested fields.
	 */
	public function testNestedFieldError() {
		$doc = new Document(array('data' => array('foo' => array('bar' => 'value'))));
		$doc->errors(array('foo.bar' => 'Something bad happened.'));

		$this->form->create($doc);
		$result = $this->form->field('foo.bar');

		$this->assertTags($result, array(
			array('div' => array()),
			'label' => array('for' => 'FooBar'), 'Foo Bar', '/label',
			'input' => array(
				'type' => 'text', 'name' => 'foo[bar]', 'id' => 'FooBar', 'value' => 'value'
			),
			'div' => array('class' => 'error'), 'Something bad happened.', '/div',
			array('/div' => array())
		));
	}

	public function testFormCreationWithNoContext() {
		$this->form = new Form(array('context' => new MockFormRenderer(array(
			'request' => new Request(array('base' => '/bbq'))
		))));
		$result = $this->form->create(null, array('url' => '/foo'));

		$this->assertTags($result, array('form' => array(
			'action' => "/bbq/foo",
			'method' => "post"
		)));
	}

	/**
	 * Tests that magic method support can be used to automatically generate a `<button />` tag
	 * based on the default string template.
	 */
	public function testButton() {
		$result = $this->form->button('Foo!', array('id' => 'bar'));
		$this->assertTags($result, array('button' => array('id' => 'bar'), 'Foo!', '/button'));

		$result = $this->form->button('Continue >', array('type' => 'submit'));
		$this->assertTags($result, array(
			'button' => array('type' => 'submit', 'id' => 'Continue'),
			'Continue &gt;',
			'/button'
		));
	}

	/**
	 * Tests that field references passed to `label()` in dot-separated format correctly translate
	 * to DOM ID values.
	 */
	public function testLabelIdGeneration() {
		$this->assertTags($this->form->label('user.name'), array(
			'label' => array('for' => 'UserName'), 'User Name', '/label'
		));
	}

	/**
	 * Test that field already defined template strings with special types (e.g. radio, checkbox,
	 * etc.) and passed customize template, and the template must apply.
	 */
	public function testRadioTypeFieldWithCustomTemplate() {
		$result = $this->form->field('name', array(
			'template' => '<span{:wrap}>{:label}: {:input}{:error}</span>',
			'type' => 'radio'
		));
		$this->assertTags($result, array(
			'span' => array(),
			'label' => array('for' => 'Name'), 'Name', '/label', ':',
			'input' => array('type' => 'radio', 'name' => 'name', 'id' => 'Name', 'value' => '1')
		));
	}

	public function testFormCreationMultipleBindings() {
		$record1 = new Record(array('model' => $this->_model, 'data' => array(
			'author_id' => '2',
			'title' => 'New post',
			'body' => 'New post body'
		)));
		$record2 = new Record(array('model' => $this->_model2, 'data' => array(
			'section' => 'New post section',
			'notes' => 'New post notes'
		)));

		$result = $this->form->create(array(
			'MockFormPost' => $record1,
			'MockFormPostInfo' => $record2
		));
		$this->assertTags($result, array(
			'form' => array('action' => "{$this->base}posts", 'method' => 'post')
		));

		$result = $this->form->text('title');
		$this->assertTags($result, array('input' => array(
			'type' => 'text', 'name' => 'title',
			'value' => 'New post', 'id' => 'MockFormPostTitle'
		)));

		$result = $this->form->text('MockFormPost.title');
		$this->assertTags($result, array('input' => array(
			'type' => 'text', 'name' => 'MockFormPost[title]',
			'value' => 'New post', 'id' => 'MockFormPostTitle'
		)));

		$result = $this->form->text('body');
		$this->assertTags($result, array('input' => array(
			'type' => 'text', 'name' => 'body',
			'value' => 'New post body', 'id' => 'MockFormPostBody'
		)));

		$result = $this->form->text('MockFormPostInfo.section');
		$this->assertTags($result, array('input' => array(
			'type' => 'text', 'name' => 'MockFormPostInfo[section]',
			'value' => 'New post section', 'id' => 'MockFormPostInfoSection'
		)));

		$result = $this->form->end();
		$this->assertTags($result, array('/form'));

		$result = $this->form->create(array('a' => $record1, 'b' => $record2));
		$this->assertTags($result, array(
			'form' => array('action' => "{$this->base}posts", 'method' => 'post')
		));

		$result = $this->form->text('title');
		$this->assertTags($result, array('input' => array(
			'type' => 'text', 'name' => 'title',
			'value' => 'New post', 'id' => 'MockFormPostTitle'
		)));

		$result = $this->form->text('a.title');
		$this->assertTags($result, array('input' => array(
			'type' => 'text', 'name' => 'a[title]',
			'value' => 'New post', 'id' => 'MockFormPostTitle'
		)));

		$result = $this->form->text('body');
		$this->assertTags($result, array('input' => array(
			'type' => 'text', 'name' => 'body',
			'value' => 'New post body', 'id' => 'MockFormPostBody'
		)));

		$result = $this->form->text('b.section');
		$this->assertTags($result, array('input' => array(
			'type' => 'text', 'name' => 'b[section]',
			'value' => 'New post section', 'id' => 'MockFormPostInfoSection'
		)));

		$result = $this->form->end();
		$this->assertTags($result, array('/form'));
	}

	public function testFormErrorMultipleBindings() {
		$record1 = new Record(array('model' => $this->_model, 'data' => array(
			'author_id' => '2',
			'title' => 'New post',
			'body' => 'New post body'
		)));
		$record2 = new Record(array('model' => $this->_model2, 'data' => array(
			'section' => 'New post section',
			'notes' => 'New post notes'
		)));

		$record1->errors(array('title' => 'Not a cool title'));
		$record2->errors(array('section' => 'Not a cool section'));

		$this->form->create(compact('record1', 'record2'));

		$result = $this->form->error('title');
		$this->assertTags($result, array(
			'div' => array('class' => 'error'), 'Not a cool title', '/div'
		));

		$result = $this->form->error('body');
		$this->assertTrue(empty($result));

		$result = $this->form->error('record1.title');
		$this->assertTags($result, array(
			'div' => array('class' => 'error'), 'Not a cool title', '/div'
		));

		$result = $this->form->error('record2.section');
		$this->assertTags($result, array(
			'div' => array('class' => 'error'), 'Not a cool section', '/div'
		));
	}

	public function testBindingByName() {
		$post = new Record(array('model' => $this->_model, 'data' => array(
			'author_id' => '2',
			'title' => 'New post',
			'body' => 'New post body'
		)));
		$info = new Record(array('model' => $this->_model2, 'data' => array(
			'section' => 'New post section',
			'notes' => 'New post notes'
		)));

		$this->form->create(compact('post', 'info'));
		$this->assertEqual($post, $this->form->binding('post'));
		$this->assertEqual($info, $this->form->binding('info'));
	}

	public function testRespondsTo() {
		$this->assertTrue($this->form->respondsTo('foobarbaz'));
		$this->assertFalse($this->form->respondsTo(0));
	}

}

?>