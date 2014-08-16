# Results

## Execute

Once you have a query object built, either through a parameterized statement or through the builder, you must then `execute()` the query and retrieve the results. Depending on the query type used, the results returned will vary. 

## Select

[DB::select] will return a [Database_Result] object which you can then iterate over. This example shows how you can iterate through the [Database_Result] using a foreach.

	$results = DB::select()->from('users')->where('verified', '=', 0)->execute();
	foreach($results as $user)
	{
		// Send reminder email to $user['email']
		echo $user['email']." needs to verify his/her account\n";
	}

### Select - `as_object()` and `as_assoc()`

When iterating over a result set, the default type will be an associative array with the column names or aliases as the keys. As an option, before calling `execute()`, you can specify to return the result rows as an object by using the `as_object()` method. The `as_object()` method takes one parameter, the name of the class of your choice, but will default to TRUE which uses the `stdClass`. Here is the example again using `stdClass`.

	$results = DB::select()->from('users')->where('verified', '=', 0)->as_object()->execute();
	foreach($results as $user)
	{
		// Send reminder email to $user->email
		echo $user->email." needs to verify his/her account\n";
	}

[!!] The method `as_assoc()` will remove the object name and return the results set back to an associative array. Since this is the default, this method is seldom required.

### Select - `as_array()`

Sometimes you will require the results as a pure array rather than as an object. The `Database_Result` method `as_array()` will return an array of all rows. 

	$results = DB::select('id', 'email')->from('users')->execute();
	$users = $results->as_array();
	foreach($users as $user)
	{
		echo 'User ID: '.$user['id'];
		echo 'User Email: '.$user['email'];
	}

It also accepts two parameters that can be very helpful: `$key` and `$value`. When passing a value to `$key` you will index the resulting array by the column specified.

	$results = DB::select('id', 'email')->from('users')->execute();
	$users = $results->as_array('id');
	foreach($users as $id => $user)
	{
		echo 'User ID: '.$id;
		echo 'User Email: '.$user['email'];
	}

The second parameter, `$value`, will reference the column specified and return that value rather than the whole row.  This is particularly useful when making `<select>` dropdowns.

	$results = DB::select('id', 'name')->from('users')->execute();
	$users = $results->as_array('id','name');
	// Show a dropdown with all users in it.
	echo Form::select('author', $users)

To return a non-associative array, leave `$key` as NULL and just pass a `$value`.

	$results = DB::select('email')->from('users')->execute();
	$users = $results->as_array(NULL, 'email');
	foreach($users as $email)
	{
		echo 'User Email: '.$email;
	}

### Select - `get()`

Sometime you only want a single value from a query. The `get()` method returns the value of the named column from the current row. The second parameter, `$default`, is used to supply a default value when the result is NULL.

	$total_users = DB::select(array(DB::expr('COUNT(`username`)'), 'total_users'))->from('users')->execute()->get('total_users', 0);

### Select - `cached()`

The mysql database driver returns a `Database_Result` that works with a MySQL Resource data type. Since this resource lives outside of PHP environment, it can't be serialized which means it also can't be cached. To get around this the `Database_Result` object has the `cached()` method that returns a `Database_Result_Cached` object of the result set. The `Database_Result_Cached` can be serialized and cached, but can take up more memory. 

[!!] NOTE: Currently, the PDO diver always returns a class of `Database_Result_Cached`, so `cached()` just returns itself.

The `cached()` function doesn't actually do any caching, it simply returns the result in a way that can be serialized and cached.  You will need to use the [Cache Module](../cache) or some other caching method.

### Select - `count()`

The `Database_Result` object implements the `Countable` Interface. The method `count()` returns the total row count in the result set. 

[!!] NOTE: This is the count of the current result set, not a count of how many records are in the database. This is important to point out especially when using `limit()` and `offset()` in your query.

[!!] For a complete list of methods available when working with a result set see [Database_Result].

## Insert

[DB::insert] returns an array of two values: the last insert id and the number of affected rows.
	
	$insert = DB::insert('tools')
		->columns(array('name', 'model', 'description'))
		->values(array('Skil 3400 10" Table Saw', '3400', 'Powerful 15 amp motor; weighs just 54-pounds'));
		
	list($insert_id, $affected_rows) = $insert->execute();

## Update & Delete

[DB::update] and [DB::delete] both return the number of affected rows as an integer.

	$rows_deleted = DB::delete('tools')->where('model', 'like', '3400')->execute();
