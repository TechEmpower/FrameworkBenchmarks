# Configuration

By default Kohana is setup to load configuration values from [config files](files/config) in the
cascading filesystem.  However, it is very easy to adapt it to load config values in other
locations/formats.

## Sources

The system is designed around the concept of **Config Sources**, which loosely means a method of
storing configuration values.

To read config from a source you need a **Config Reader**. Similarly, to write config to a source
you need a **Config Writer**.

Implementing them is as simple as extending the
[Kohana_Config_Reader] / [Kohana_Config_Writer] interfaces:

	class Kohana_Config_Database_Reader implements Kohana_Config_Reader
	class Kohana_Config_Database_Writer extends Kohana_Config_Database_Reader implements Kohana_Config_Writer

You'll notice in the above example that the Database Writer extends the Database Reader.
This is the convention with config sources, the reasoning being that if you can write to a
source chances are you can also read from it as well. However, this convention is not enforced
and is left to the developer's discretion.

## Groups

In order to aid organisation config values are split up into logical "groups".  For example,
database related settings go in a `database` group, and session related settings go in a
`session` group.

How these groups are stored/organised is up to the config source.  For example, the file source
puts different config groups into different files (`database.php`, `session.php`) whereas
the database source uses a column to distinguish between groups.

To load a config group simply call `Kohana::$config->load()` with the name of the group you wish to load:

	$config = Kohana::$config->load('my_group');

`load()` will return an instance of [Config_Group] which encapsulates the config values and ensures
that any modifications made will be passed back to the config writers.

To get a config value from a [Config_Group] object simply call [Config_Group::get]:

	$config = Kohana::$config->load('my_group');
	$value  = $config->get('var');

To modify a value call [Config_Group::set]:

	$config = Kohana::$config->load('my_group');
	$config->set('var', 'new_value');

### Alternative methods for getting / setting config

In addition to the methods described above you can also access config values using dots to outline a path
from the config group to the value you want:

	// Config file: database.php
	return array(
		'default' => array(
			'connection' => array(
				'hostname' => 'localhost'
			)
		)
	);

	// Code which needs hostname:
	$hostname = Kohana::$config->load('database.default.connection.hostname');


Which is equivalent to:

	$config = Kohana::$config->load('database')->get('default');

	$hostname = $config['connection']['hostname'];

Obviously this method is a lot more compact than the original. However, please bear in mind that using
`dot.notation` is a _lot_ slower than calling `get()` and traversing the array yourself.  Dot notation
can be useful if you only need one specific variable, but otherwise it's best to use `get()`.

As [Config_Group] extends [Array_Object](http://php.net/manual/en/class.arrayobject.php) you can also use array
syntax to get/set config vars:

	$config = Kohana::$config->load('database');

	// Getting the var
	$hostname = $config['default']['connection']['hostname'];

	// Setting the var
	$config['default']['connection']['hostname'] = '127.0.0.1';

Again, this syntax is more costly than calling `get()` / `set()`.

## Config Merging

One of the useful features of the config system is config group merging. This works in a similar way
to the cascading filesystem, with configuration from lower sources lower down the source stack being
merged with sources further up the stack.

If two sources contain the same config variables then the one from the source further up the stack will
override the one from the "lower" source.  However, if the source from higher up the stack does not contain
a particular config variable but a source lower down the stack does then the value from the lower source will
be used.

The position of sources in the stack is determined by how they are loaded in your bootstrap.
By default when you load a source it is pushed to the top of a stack:

    // Stack: <empty>
	Kohana::$config->attach(new Config_File);
	// Stack: Config_File
	Kohana::$config->attach(new Config_Database);
	// Stack: Config_Database, Config_File

In the example above, any config values found in the database will override those found in the filesystem.
For example, using the setup outlined above:

	// Configuration in the filesystem:
		email:
			sender:
				email: my.awesome.address@example.com
				name:  Unknown
			method: smtp

	// Configuration in the database:
		email:
			sender:
				email: my.supercool.address@gmail.com
				name:  Kohana Bot

	// Configuration returned by Kohana::$config->load('email')
		email:
			sender:
				email: my.supercool.address@gmail.com
				name:  Kohana Bot
			method: smtp

[!!] **Note:** The above syntax is simply pseudo code to illustrate the concept of config merging.

On some occasions you may want to append a config source to the bottom of the stack, to do this pass `FALSE`
as the second parameter to `attach()`:

	// Stack: <empty>
	Kohana::$config->attach(new Config_File);
	// Stack: Config_File
	Kohana::$config->attach(new Config_Database, FALSE);
	// Stack: Config_File, Config_Database

In this example, any values found in the filesystem will override those found in the db. For example:

	// Configuration in the filesystem:
		email:
			sender:
				email: my.awesome.address@example.com
				name:  Unknown
			method: smtp

	// Configuration in the database:
		email:
			sender:
				email: my.supercool.address@gmail.com
				name:  Kohana Bot

	// Configuration returned by Kohana::$config->load('email')
		email:
			sender:
				email: my.awesome.address@example.com
				name:  Unknown
			method: smtp

## Using different config sources based on the environment

In some situations you'll need to use different config values depending on which state `Kohana::$environment`
is in. Unit testing is a prime example of such a situation. Most setups have two databases; one for normal
development and a separate one for unit testing (to isolate the tests from your development).

In this case you still need access to the config settings stored in the `config` directory as it contains generic
settings that are needed whatever environment your application is in (e.g. encryption settings),
so replacing the default `Config_File` source isn't really an option.

To get around this you can attach a separate config file reader which loads its config from a subdir of `config` called
"testing":

	Kohana::$config->attach(new Config_File);

	Kohana::$config->attach(new Config_Database);

	if (Kohana::$environment === Kohana::TESTING)
	{
		Kohana::$config->attach(new Config_File('config/testing'));
	}

During normal development the config source stack looks like `Config_Database, Config_File('config')`.  However,
when `Kohana::$environment === Kohana::TESTING` the stack looks like `Config_File('config/testing'), Config_Database, Config_File('config')`