# Parameterized Statements

Using parameterized statements allows you to write SQL queries manually while still escaping the query values automatically to prevent [SQL injection](http://wikipedia.org/wiki/SQL_Injection). Creating a query is simple:

    $query = DB::query(Database::SELECT, 'SELECT * FROM users WHERE username = :user');

The [DB::query] method is just a shortcut that creates a new [Database_Query] class for us, to allow method chaining. The query contains a `:user` parameter, which we will get to in a second.

The first parameter of [DB::query] is the type of query.  It should be `Database::SELECT`, `Database::INSERT`, `Database::UPDATE`, or `Database::DELETE`.  This is done for compatibility reasons for drivers, and to easily determine what `execute()` should return.

The second parameter is the query itself.  Rather than trying to concatenate your query and variables together, you should make use of [Database_Query::param].  This will make your queries much easier to mantain, and will escape the values to prevent [SQL injection](http://wikipedia.org/wiki/SQL_Injection). 

## Parameters

Our example query earlier contains a `:user` parameter, which we can assign to a value using [Database_Query::param] like so:

    $query->param(':user', 'john');

[!!] Parameter names can be any unique string, as they are replaced using [strtr](http://php.net/strtr). It is highly recommended to **not** use dollars signs as parameter names to prevent confusion.  Colons are commonly used.

You can also update the `:user` parameter by calling [Database_Query::param] again:

    $query->param(':user', $_GET['search']);

If you want to set multiple parameters at once, you can use [Database_Query::parameters].
	
	$query = DB::query(Database::SELECT, 'SELECT * FROM users WHERE username = :user AND status = :status');

	$query->parameters(array(
		':user' => 'john',
		':status' => 'active',
	));

It is also possible to bind a parameter to a variable, using a [variable reference]((http://php.net/language.references.whatdo)). This can be extremely useful when running the same query many times:

    $query = DB::query(Database::INSERT, 'INSERT INTO users (username, password) VALUES (:user, :pass)')
        ->bind(':user', $username)
        ->bind(':pass', $password);

    foreach ($new_users as $username => $password)
    {
        $query->execute();
    }

In the above example, the variables `$username` and `$password` are changed for every loop of the `foreach` statement. When the parameter changes, it effectively changes the `:user` and `:pass` query parameters. Careful parameter binding can save a lot of code when it is used properly.

The only difference between `param()` and `bind()` is that `bind()` passes the variable by reference rather than by assignment (copied), so future changes to the variable can be "seen" by the query. 

[!!] Although all parameters are escaped to prevent SQL injection, it is still a good idea to validate/sanitize your input.

## Display the raw query

If you want to display the SQL that will be executed, you can simply echo the query:

    echo $query;
    // Should display:
    // SELECT * FROM users WHERE username = 'john'

## Executing

Once you have assigned something to each of the parameters, you can execute the query using `execute()` and use [the results](results).

    $result = $query->execute();

To use a different database [config group](config) pass either the name or the config object to `execute()`.

	$result = $query->execute('config_name')