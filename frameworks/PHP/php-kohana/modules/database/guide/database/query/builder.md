# Query Builder

Creating queries dynamically using objects and methods allows queries to be written very quickly in an agnostic way. Query building also adds identifier (table and column name) quoting, as well as value quoting.

## Select

Each type of database query is represented by a different class, each with their own methods. For instance, to create a SELECT query, we use [DB::select] which is a shortcut to return a new [Database_Query_Builder_Select] object:

    $query = DB::select();

Query Builder methods return a reference to itself so that method chaining may be used. Select queries ussually require a table and they are referenced using the `from()` method. The `from()` method takes one parameter which can be the table name (string), an array of two strings (table name and alias), or an object (See Subqueries in the Advanced Queries section below). 

    $query = DB::select()->from('users');

Limiting the results of queries is done using the `where()`, `and_where()` and `or_where()` methods. These methods take three parameters: a column, an operator, and a value. 

    $query = DB::select()->from('users')->where('username', '=', 'john');

Multiple `where()` methods may be used to string together multiple clauses connected by the boolean operator in the method's prefix. The `where()` method is a wrapper that just calls `and_where()`. 

    $query = DB::select()->from('users')->where('username', '=', 'john')->or_where('username', '=', 'jane');
	
You can use any operator you want.  Examples include `IN`, `BETWEEN`, `>`, `=<`, `!=`, etc.  Use an array for operators that require more than one value.

	$query = DB::select()->from('users')->where('logins', '<=', 1);
	
	$query = DB::select()->from('users')->where('logins', '>', 50);
	
	$query = DB::select()->from('users')->where('username', 'IN', array('john','mark','matt'));
	
	$query = DB::select()->from('users')->where('joindate', 'BETWEEN', array($then, $now));

By default, [DB::select] will select all columns (`SELECT * ...`), but you can also specify which columns you want returned by passing parameters to [DB::select]:

    $query = DB::select('username', 'password')->from('users')->where('username', '=', 'john');

Now take a minute to look at what this method chain is doing. First, we create a new selection object using the [DB::select] method. Next, we set table(s) using the `from()` method. Last, we search for a specific records using the `where()` method. We can display the SQL that will be executed by casting the query to a string:

    echo Debug::vars((string) $query);
    // Should display:
    // SELECT `username`, `password` FROM `users` WHERE `username` = 'john'

Notice how the column and table names are automatically escaped, as well as the values? This is one of the key benefits of using the query builder.

### Select - AS (column aliases)

It is also possible to create `AS` aliases when selecting, by passing an array as each parameter to [DB::select]:

    $query = DB::select(array('username', 'u'), array('password', 'p'))->from('users');

This query would generate the following SQL:

    SELECT `username` AS `u`, `password` AS `p` FROM `users`

### Select - DISTINCT

Unique column values may be turned on or off (default) by passing TRUE or FALSE, respectively, to the `distinct()` method.

    $query = DB::select('username')->distinct(TRUE)->from('posts');

This query would generate the following SQL:

    SELECT DISTINCT `username` FROM `posts`

### Select - LIMIT & OFFSET

When querying large sets of data, it is often better to limit the results and page through the data one chunk at a time. This is done using the `limit()` and `offset()` methods.

    $query = DB::select()->from(`posts`)->limit(10)->offset(30);

This query would generate the following SQL:

    SELECT * FROM `posts` LIMIT 10 OFFSET 30

### Select - ORDER BY

Often you will want the results in a particular order and rather than sorting the results, it's better to have the results returned to you in the correct order. You can do this by using the order_by() method. It takes the column name and an optional direction string as the parameters. Multiple `order_by()` methods can be used to add additional sorting capability.

    $query = DB::select()->from(`posts`)->order_by(`published`, `DESC`);

This query would generate the following SQL:

    SELECT * FROM `posts` ORDER BY `published` DESC

[!!] For a complete list of methods available while building a select query see [Database_Query_Builder_Select].

## Insert

To create records into the database, use [DB::insert] to create an INSERT query, using `values()` to pass in the data:

    $query = DB::insert('users', array('username', 'password'))->values(array('fred', 'p@5sW0Rd'));

This query would generate the following SQL:

    INSERT INTO `users` (`username`, `password`) VALUES ('fred', 'p@5sW0Rd')

[!!] For a complete list of methods available while building an insert query see [Database_Query_Builder_Insert].

## Update

To modify an existing record, use [DB::update] to create an UPDATE query:

    $query = DB::update('users')->set(array('username' => 'jane'))->where('username', '=', 'john');

This query would generate the following SQL:

    UPDATE `users` SET `username` = 'jane' WHERE `username` = 'john'

[!!] For a complete list of methods available while building an update query see [Database_Query_Builder_Update].

## Delete

To remove an existing record, use [DB::delete] to create a DELETE query:

    $query = DB::delete('users')->where('username', 'IN', array('john', 'jane'));

This query would generate the following SQL:

    DELETE FROM `users` WHERE `username` IN ('john', 'jane')

[!!] For a complete list of methods available while building a delete query see [Database_Query_Builder_Delete].

## Advanced Queries

### Joins

Multiple tables can be joined using the `join()` and `on()` methods. The `join()` method takes two parameters. The first is either a table name, an array containing the table and alias, or an object (subquery or expression). The second parameter is the join type: LEFT, RIGHT, INNER, etc.

The `on()` method sets the conditions for the previous `join()` method and is very similar to the `where()` method in that it takes three parameters; left column (name or object), an operator, and the right column (name or object). Multiple `on()` methods may be used to supply multiple conditions and they will be appended with an 'AND' operator. 

    // This query will find all the posts related to "smith" with JOIN
    $query = DB::select('authors.name', 'posts.content')->from('authors')->join('posts')->on('authors.id', '=', 'posts.author_id')->where('authors.name', '=', 'smith');

This query would generate the following SQL:

    SELECT `authors`.`name`, `posts`.`content` FROM `authors` JOIN `posts` ON (`authors`.`id` = `posts`.`author_id`) WHERE `authors`.`name` = 'smith'

If you want to do a LEFT, RIGHT or INNER JOIN you would do it like this `join('colum_name', 'type_of_join')`:

    // This query will find all the posts related to "smith" with LEFT JOIN
    $query = DB::select()->from('authors')->join('posts', 'LEFT')->on('authors.id', '=', 'posts.author_id')->where('authors.name', '=', 'smith');

This query would generate the following SQL:

    SELECT `authors`.`name`, `posts`.`content` FROM `authors` LEFT JOIN `posts` ON (`authors`.`id` = `posts`.`author_id`) WHERE `authors`.`name` = 'smith'

[!!] When joining multiple tables with similar column names, it's best to prefix the columns with the table name or table alias to avoid errors. Ambiguous column names should also be aliased so that they can be referenced easier.

### Database Functions

Eventually you will probably run into a situation where you need to call `COUNT` or some other database function within your query. The query builder supports these functions using the `Database_Expression` class:

    $query = DB::select(array(DB::expr('COUNT(`username`)'), 'total_users'))->from('users');

This looks almost exactly the same as a standard `AS` alias, but note how the column name is put in a call to `DB::expr()`. Any time `DB::expr()` is used, the column name will **not** be escaped. This query would generate the following SQL:

    SELECT COUNT(`username`) AS `total_users` FROM `users`

[!!] When building complex queries and you need to get a count of the total rows that will be returned, build the expression with an empty column list first. Then clone the query and add the COUNT function to one copy and the columns list to the other. This will cut down on the total lines of code and make updating the query easier.

    $query = DB::select()->from('users')
        ->join('posts')->on('posts.username', '=', 'users.username')
        ->where('users.active', '=', TRUE)
        ->where('posts.created', '>=', $yesterday);
    
    $total = clone $query;
    $total->select(array(DB::expr('COUNT( DISTINCT `username`)'), 'unique_users'));
    $query->select('posts.username')->distinct();

### Aggregate Functions

Aggregate functions like `COUNT()`, `SUM()`, `AVG()`, etc. will most likely be used with the `group_by()` and possibly the `having()` methods in order to group and filter the results on a set of columns.

    $query = DB::select('username', array(DB::expr('COUNT(`id`)'), 'total_posts')
        ->from('posts')->group_by('username')->having('total_posts', '>=', 10);

This will generate the following query:

    SELECT `username`, COUNT(`id`) AS `total_posts` FROM `posts` GROUP BY `username` HAVING `total_posts` >= 10

### Subqueries

Query Builder objects can be passed as parameters to many of the methods to create subqueries. Let's take the previous example query and pass it to a new query.

    $sub = DB::select('username', array(DB::expr('COUNT(`id`)'), 'total_posts')
        ->from('posts')->group_by('username')->having('total_posts', '>=', 10);
    
    $query = DB::select('profiles.*', 'posts.total_posts')->from('profiles')
        ->join(array($sub, 'posts'), 'INNER')->on('profiles.username', '=', 'posts.username');

This will generate the following query:

    SELECT `profiles`.*, `posts`.`total_posts` FROM `profiles` INNER JOIN
    ( SELECT `username`, COUNT(`id`) AS `total_posts` FROM `posts` GROUP BY `username` HAVING `total_posts` >= 10 ) AS posts
    ON `profiles`.`username` = `posts`.`username`

Insert queries can also use a select query for the input values

    $sub = DB::select('username', array(DB::expr('COUNT(`id`)'), 'total_posts')
        ->from('posts')->group_by('username')->having('total_posts', '>=', 10);
    
    $query = DB::insert('post_totals', array('username', 'posts'))->select($sub);

This will generate the following query:

    INSERT INTO `post_totals` (`username`, `posts`) 
    SELECT `username`, COUNT(`id`) AS `total_posts` FROM `posts` GROUP BY `username` HAVING `total_posts` >= 10 

### Boolean Operators and Nested Clauses 

Multiple Where and Having clauses are added to the query with Boolean operators connecting each expression. The default operator for both methods is AND which is the same as the and_ prefixed method. The OR operator can be specified by prefixing the methods with or_. Where and Having clauses can be nested or grouped by post fixing either method with _open and then followed by a method with a _close. 

    $query = DB::select()->from('users')
        ->where_open()
            ->or_where('id', 'IN', $expired)
            ->and_where_open()
                ->where('last_login', '<=', $last_month)
                ->or_where('last_login', 'IS', NULL)
            ->and_where_close()
        ->where_close()
        ->and_where('removed','IS', NULL);

This will generate the following query:

    SELECT * FROM `users` WHERE ( `id` IN (1, 2, 3, 5) OR ( `last_login` <= 1276020805 OR `last_login` IS NULL ) ) AND `removed` IS NULL 

### Database Expressions

There are cases were you need a complex expression or other database functions, which you don't want the Query Builder to try and escape. In these cases, you will need to use a database expression created with [DB::expr].  **A database expression is taken as direct input and no escaping is performed.**

    $query = DB::update('users')->set(array('login_count' => DB::expr('login_count + 1')))->where('id', '=', $id);

This will generate the following query, assuming `$id = 45`:

    UPDATE `users` SET `login_count` = `login_count` + 1 WHERE `id` = 45

Another example to calculate the distance of two geographical points:

    $query = DB::select(array(DB::expr('degrees(acos(sin(radians('.$lat.')) * sin(radians(`latitude`)) + cos(radians('.$lat.')) * cos(radians(`latitude`)) * cos(radians(abs('.$lng.' - `longitude`))))) * 69.172'), 'distance'))->from('locations');

[!!] You must validate or escape any user input inside of DB::expr as it will obviously not be escaped it for you.

## Executing

Once you are done building, you can execute the query using `execute()` and use [the results](results).

    $result = $query->execute();

To use a different database [config group](config) pass either the name or the config object to `execute()`.

	$result = $query->execute('config_name')