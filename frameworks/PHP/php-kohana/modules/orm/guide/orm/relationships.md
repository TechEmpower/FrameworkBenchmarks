# Relationships

Kohana ORM supports four types of object relationships: `belongs_to`, `has_many`, `has_many "through"` and `has_one`. The `has_many "through"` relationship can be used to function like Active Record's `has_many_and_belongs_to` relationship type.

## belongs_to

A `belongs_to` relation should be used when you have one model that belongs to another. For example, a `Child` model belongs_to a `Parent` or a `Flag` model `belongs_to` a `Country`.

This is the base `belongs_to` relationship:

	protected $_belongs_to = array(
		'[alias name]' => array(
			'model'       => '[model name]',
			'foreign_key' => '[column]',
		),
	);

You can omit any or all of the keys/values in the array on the right, in which case defaults are used:

	protected $_belongs_to = array('[alias name]' => array());

The **alias name** is what is used to access the related model in your code. If you had a `Post` model that belonged to a `User` model and wished to use the default values of the `belongs_to` configuration then your code would look like this:

	protected $_belongs_to = array('user' => array());

To access the user model, you would use `$post->user`.  Since we're using the defaults above, the alias name will be used for the model name, and the foreign key in the posts table will be the alias name followed by `_id`, in this case it would be `user_id`. (You can change the `_id` suffix by modifying the `$foreign_key_suffix` variable in the model.)

Let's say your `Post` database table schema doesn't have a `user_id` column but instead has an `author_id` column which is a foreign key for a record in the `User` table. You could use code like this:

	protected $_belongs_to = array(
		'user' => array(
			'foreign_key' => 'author_id',
		),
	);

If you wanted access a post's author by using code like `$post->author` then you would simply need to change the alias and add the `model` index:

	protected $_belongs_to = array(
		'author' => array(
			'model'       => 'User',
		),
	);

## has_many

The standard `has_many` relationship will likely fall on the other side of a `belongs_to` relationship.  In the above examples, a post belongs to a user.  From the user's perspective, a user has many posts. A has_many relationship is defined below:

	protected $_has_many = array(
		'[alias name]' => array(
			'model'       => '[model name]',
			'foreign_key' => '[column]',
		),
	);

Again, you can omit all keys in the right array to use the defaults:

	protected $_has_many = array('[alias name]' => array());

For our user and post example, this would look like the following in the user model:

	protected $_has_many = array('posts' => array());

Using the above, the posts could be access using `$user->posts->find_all()`.  Notice the `find_all()` used in this example. With `belongs_to` and `has_one` relationship types, the model is already loaded with necessary data.  For `has_many` relationships, however, you may want to limit the number of results or add additional conditions to the SQL query; you can do so prior to the `find_all()`.

The model name used by default will be the singular name of the alias using the `inflector` class.  In this case, `posts` uses `post` as the model name.  The foreign key used by default is the owner model's name followed by `_id`.  In this case, the foreign key will be `user_id` and it must exist in the posts table as before.

Let's assume now you want to access the posts using the name `stories` instead, and are still using the `author_id` key as in the `belongs_to` example.  You would define your has_many relationship as:

	protected $_has_many = array(
		'stories' => array(
			'model'       => 'Post',
			'foreign_key' => 'author_id',
		),
	);

## has_one

A `has_one` relationship is almost identical to a `has_many` relationship.  In a `has_one` relationship, there can be 1 and only 1 relationship (rather than 1 or more in a has_many). If a user can only have one post or story, rather than many then the code would look like this:

	protected $_has_one = array(
		'story' => array(
			'model'       => 'Post',
			'foreign_key' => 'author_id',
		),
	);

## has_many "through"

A `has_many "through"` relationship is used for many-to-many relationships.  For instance, let's assume now we have an additional model, called `Category`.  Posts may belong to more than one category, and each category may have more than one post.  To link them together, an additional table is needed with columns for a `post_id` and a `category_id` (sometimes called a pivot table).  We'll name the model for this `Post_Category` and the corresponding table `categories_posts`.

To define the `has_many` "through" relationship, the same syntax for standard has_many relationships is used with the addition of a 'through' parameter.  Let's assume we're working with the Post model:

	protected $_has_many = array(
		'categories' => array(
			'model'   => 'Category',
			'through' => 'categories_posts',
		),
	);

In the Category model:

	protected $_has_many = array(
		'posts' => array(
			'model'   => 'Post',
			'through' => 'categories_posts',
		),
	);

To access the categories and posts, you simply use `$post->categories->find_all()` and `$category->posts->find_all()`

Methods are available to check for, add, and remove relationships for many-to-many relationships.  Let's assume you have a $post model loaded, and a $category model loaded as well.  You can check to see if the $post is related to this $category with the following call:

	$post->has('categories', $category);

The first parameter is the alias name to use (in case your post model has more than one relationship to the category model) and the second is the model to check for a relationship with.

Assuming you want to add the relationship (by creating a new record in the categories_posts table), you would simply do:

	$post->add('categories', $category);

To remove:

	$post->remove('categories', $category);
