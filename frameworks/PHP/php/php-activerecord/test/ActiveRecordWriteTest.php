<?php
include 'helpers/config.php';
use ActiveRecord\DateTime;

class DirtyAuthor extends ActiveRecord\Model
{
	static $table = 'authors';
	static $before_save = 'before_save';

	public function before_save()
	{
		$this->name = 'i saved';
	}
};

class AuthorWithoutSequence extends ActiveRecord\Model
{
	static $table = 'authors';
	static $sequence = 'invalid_seq';
}

class AuthorExplicitSequence extends ActiveRecord\Model
{
	static $sequence = 'blah_seq';
}

class ActiveRecordWriteTest extends DatabaseTest
{
	private function make_new_book_and($save=true)
	{
		$book = new Book();
		$book->name = 'rivers cuomo';
		$book->special = 1;

		if ($save)
			$book->save();

		return $book;
	}

	public function test_save()
	{
		$venue = new Venue(array('name' => 'Tito'));
		$venue->save();
	}

	public function test_insert()
	{
		$author = new Author(array('name' => 'Blah Blah'));
		$author->save();
		$this->assert_not_null(Author::find($author->id));
	}

	/**
	 * @expectedException ActiveRecord\DatabaseException
	 */
	public function test_insert_with_no_sequence_defined()
	{
		if (!$this->conn->supports_sequences())
			throw new ActiveRecord\DatabaseException('');

		AuthorWithoutSequence::create(array('name' => 'Bob!'));
	}

	public function test_insert_should_quote_keys()
	{
		$author = new Author(array('name' => 'Blah Blah'));
		$author->save();
		$this->assert_true(strpos($author->connection()->last_query,$author->connection()->quote_name('updated_at')) !== false);
	}

	public function test_save_auto_increment_id()
	{
		$venue = new Venue(array('name' => 'Bob'));
		$venue->save();
		$this->assert_true($venue->id > 0);
	}

	public function test_sequence_was_set()
	{
		if ($this->conn->supports_sequences())
			$this->assert_equals($this->conn->get_sequence_name('authors','author_id'),Author::table()->sequence);
		else
			$this->assert_null(Author::table()->sequence);
	}

	public function test_sequence_was_explicitly_set()
	{
		if ($this->conn->supports_sequences())
			$this->assert_equals(AuthorExplicitSequence::$sequence,AuthorExplicitSequence::table()->sequence);
		else
			$this->assert_null(Author::table()->sequence);
	}

	public function test_delete()
	{
		$author = Author::find(1);
		$author->delete();

		$this->assert_false(Author::exists(1));
	}

	public function test_delete_by_find_all()
	{
		$books = Book::all();

		foreach ($books as $model)
			$model->delete();

		$res = Book::all();
		$this->assert_equals(0,count($res));
	}

	public function test_update()
	{
		$book = Book::find(1);
		$new_name = 'new name';
		$book->name = $new_name;
		$book->save();

		$this->assert_same($new_name, $book->name);
		$this->assert_same($new_name, $book->name, Book::find(1)->name);
	}

	public function test_update_should_quote_keys()
	{
		$book = Book::find(1);
		$book->name = 'new name';
		$book->save();
		$this->assert_true(strpos($book->connection()->last_query,$book->connection()->quote_name('name')) !== false);
	}

	public function test_update_attributes()
	{
		$book = Book::find(1);
		$new_name = 'How to lose friends and alienate people'; // jax i'm worried about you
		$attrs = array('name' => $new_name);
		$book->update_attributes($attrs);

		$this->assert_same($new_name, $book->name);
		$this->assert_same($new_name, $book->name, Book::find(1)->name);
	}

	/**
	 * @expectedException ActiveRecord\UndefinedPropertyException
	 */
	public function test_update_attributes_undefined_property()
	{
		$book = Book::find(1);
		$book->update_attributes(array('name' => 'new name', 'invalid_attribute' => true , 'another_invalid_attribute' => 'blah'));
	}

	public function test_update_attribute()
	{
		$book = Book::find(1);
		$new_name = 'some stupid self-help book';
		$book->update_attribute('name', $new_name);

		$this->assert_same($new_name, $book->name);
		$this->assert_same($new_name, $book->name, Book::find(1)->name);
	}

	/**
	 * @expectedException ActiveRecord\UndefinedPropertyException
	 */
	public function test_update_attribute_undefined_property()
	{
		$book = Book::find(1);
		$book->update_attribute('invalid_attribute', true);
	}

	public function test_save_null_value()
	{
		$book = Book::first();
		$book->name = null;
		$book->save();
		$this->assert_same(null,Book::find($book->id)->name);
	}

	public function test_save_blank_value()
	{
		// oracle doesn't do blanks. probably an option to enable?
		if ($this->conn instanceof ActiveRecord\OciAdapter)
			return;

		$book = Book::find(1);
		$book->name = '';
		$book->save();
		$this->assert_same('',Book::find(1)->name);
	}

	public function test_dirty_attributes()
	{
		$book = $this->make_new_book_and(false);
		$this->assert_equals(array('name','special'),array_keys($book->dirty_attributes()));
	}

	public function test_dirty_attributes_cleared_after_saving()
	{
		$book = $this->make_new_book_and();
		$this->assert_true(strpos($book->table()->last_sql,'name') !== false);
		$this->assert_true(strpos($book->table()->last_sql,'special') !== false);
		$this->assert_equals(null,$book->dirty_attributes());
	}

	public function test_dirty_attributes_cleared_after_inserting()
	{
		$book = $this->make_new_book_and();
		$this->assert_equals(null,$book->dirty_attributes());
	}

	public function test_no_dirty_attributes_but_still_insert_record()
	{
		$book = new Book;
		$this->assert_equals(null,$book->dirty_attributes());
		$book->save();
		$this->assert_equals(null,$book->dirty_attributes());
		$this->assert_not_null($book->id);
	}

	public function test_dirty_attributes_cleared_after_updating()
	{
		$book = Book::first();
		$book->name = 'rivers cuomo';
		$book->save();
		$this->assert_equals(null,$book->dirty_attributes());
	}

	public function test_dirty_attributes_after_reloading()
	{
		$book = Book::first();
		$book->name = 'rivers cuomo';
		$book->reload();
		$this->assert_equals(null,$book->dirty_attributes());
	}

	public function test_dirty_attributes_with_mass_assignment()
	{
		$book = Book::first();
		$book->set_attributes(array('name' => 'rivers cuomo'));
		$this->assert_equals(array('name'), array_keys($book->dirty_attributes()));
	}

	public function test_timestamps_set_before_save()
	{
		$author = new Author;
		$author->save();
		$this->assert_not_null($author->created_at, $author->updated_at);

		$author->reload();
		$this->assert_not_null($author->created_at, $author->updated_at);
	}

	public function test_timestamps_updated_at_only_set_before_update()
	{
		$author = new Author();
		$author->save();
		$created_at = $author->created_at;
		$updated_at = $author->updated_at;
		sleep(1);

		$author->name = 'test';
		$author->save();

		$this->assert_not_null($author->updated_at);
		$this->assert_same($created_at, $author->created_at);
		$this->assert_not_equals($updated_at, $author->updated_at);
	}

	public function test_create()
	{
		$author = Author::create(array('name' => 'Blah Blah'));
		$this->assert_not_null(Author::find($author->id));
	}

	public function test_create_should_set_created_at()
	{
		$author = Author::create(array('name' => 'Blah Blah'));
		$this->assert_not_null($author->created_at);
	}

	/**
	 * @expectedException ActiveRecord\ActiveRecordException
	 */
	public function test_update_with_no_primary_key_defined()
	{
		Author::table()->pk = array();
		$author = Author::first();
		$author->name = 'blahhhhhhhhhh';
		$author->save();
	}

	/**
	 * @expectedException ActiveRecord\ActiveRecordException
	 */
	public function test_delete_with_no_primary_key_defined()
	{
		Author::table()->pk = array();
		$author = author::first();
		$author->delete();
	}

	public function test_inserting_with_explicit_pk()
	{
		$author = Author::create(array('author_id' => 9999, 'name' => 'blah'));
		$this->assert_equals(9999,$author->author_id);
	}

	/**
	 * @expectedException ActiveRecord\ReadOnlyException
	 */
	public function test_readonly()
	{
		$author = Author::first(array('readonly' => true));
		$author->save();
	}

	public function test_modified_attributes_in_before_handlers_get_saved()
	{
		$author = DirtyAuthor::first();
		$author->encrypted_password = 'coco';
		$author->save();
		$this->assert_equals('i saved',DirtyAuthor::find($author->id)->name);
	}

	public function test_is_dirty()
	{
		$author = Author::first();
		$this->assert_equals(false,$author->is_dirty());

		$author->name = 'coco';
		$this->assert_equals(true,$author->is_dirty());
	}

	public function test_set_date_flags_dirty()
	{
		$author = Author::create(array('some_date' => new DateTime()));
		$author = Author::find($author->id);
		$author->some_date->setDate(2010,1,1);
		$this->assert_has_keys('some_date', $author->dirty_attributes());
	}

	public function test_set_date_flags_dirty_with_php_datetime()
	{
		$author = Author::create(array('some_date' => new \DateTime()));
		$author = Author::find($author->id);
		$author->some_date->setDate(2010,1,1);
		$this->assert_has_keys('some_date', $author->dirty_attributes());
	}

	public function test_delete_all_with_conditions_as_string()
	{
		$num_affected = Author::delete_all(array('conditions' => 'parent_author_id = 2'));
		$this->assert_equals(2, $num_affected);
	}

	public function test_delete_all_with_conditions_as_hash()
	{
		$num_affected = Author::delete_all(array('conditions' => array('parent_author_id' => 2)));
		$this->assert_equals(2, $num_affected);
	}

	public function test_delete_all_with_conditions_as_array()
	{
		$num_affected = Author::delete_all(array('conditions' => array('parent_author_id = ?', 2)));
		$this->assert_equals(2, $num_affected);
	}

	public function test_delete_all_with_limit_and_order()
	{
		if (!$this->conn->accepts_limit_and_order_for_update_and_delete())
			$this->mark_test_skipped('Only MySQL & Sqlite accept limit/order with UPDATE clause');

		$num_affected = Author::delete_all(array('conditions' => array('parent_author_id = ?', 2), 'limit' => 1, 'order' => 'name asc'));
		$this->assert_equals(1, $num_affected);
		$this->assert_true(strpos(Author::table()->last_sql, 'ORDER BY name asc LIMIT 1') !== false);
	}

	public function test_update_all_with_set_as_string()
	{
		$num_affected = Author::update_all(array('set' => 'parent_author_id = 2'));
		$this->assert_equals(2, $num_affected);
		$this->assert_equals(4, Author::count_by_parent_author_id(2));
	}

	public function test_update_all_with_set_as_hash()
	{
		$num_affected = Author::update_all(array('set' => array('parent_author_id' => 2)));
		$this->assert_equals(2, $num_affected);
	}

	/**
	 * TODO: not implemented
	public function test_update_all_with_set_as_array()
	{
		$num_affected = Author::update_all(array('set' => array('parent_author_id = ?', 2)));
		$this->assert_equals(2, $num_affected);
	}
	 */

	public function test_update_all_with_conditions_as_string()
	{
		$num_affected = Author::update_all(array('set' => 'parent_author_id = 2', 'conditions' => 'name = "Tito"'));
		$this->assert_equals(1, $num_affected);
	}

	public function test_update_all_with_conditions_as_hash()
	{
		$num_affected = Author::update_all(array('set' => 'parent_author_id = 2', 'conditions' => array('name' => "Tito")));
		$this->assert_equals(1, $num_affected);
	}

	public function test_update_all_with_conditions_as_array()
	{
		$num_affected = Author::update_all(array('set' => 'parent_author_id = 2', 'conditions' => array('name = ?', "Tito")));
		$this->assert_equals(1, $num_affected);
	}

	public function test_update_all_with_limit_and_order()
	{
		if (!$this->conn->accepts_limit_and_order_for_update_and_delete())
			$this->mark_test_skipped('Only MySQL & Sqlite accept limit/order with UPDATE clause');

		$num_affected = Author::update_all(array('set' => 'parent_author_id = 2', 'limit' => 1, 'order' => 'name asc'));
		$this->assert_equals(1, $num_affected);
		$this->assert_true(strpos(Author::table()->last_sql, 'ORDER BY name asc LIMIT 1') !== false);
	}
};
