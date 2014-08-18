
The console package contains a set of classes required to route and dispatch
incoming console requests. Moreover it contains the console front-controller
file (`lithium.php`) as well as wrappers for both *nix and Windows environments
(`li3` and `li3.bat` respectively), allowing to easily invoke the
console front-controller from the command line.

A command is to the command line what an action controller is to the HTTP
request/response flow. In that commands are quite similar to controllers.
Commands don't leverage the full MVC as they don't utilize views, but
directly interact with the user through `in()` and `out()`.

Lithium itself provides amongst others commands for creating new applications
or parts thereof. However commands can also be provided through other libraries
or by your application. Commands running in the application context will have
complete access to your application. This is especially useful to reuse
existing logic in an application's model when creating a command to be run as
i.e. a cron-job.

### Invoking the front-controller

You invoke the console front-controller through one of the wrappers
provided, as shown below. The examples shown are relative to the root directory
of a standard Lithium  distribution. The first is for users on a *nix command
line the second for users on a Windows system. Please note that the preceding
`$` in examples always indicates things that you enter on the command line.

{{{
$ libraries/lithium/console/li3
$ libraries/lithium/console/li3.bat
}}}

However it is recommended you add the path containing the wrapper to the paths
searched by your system. This is `$PATH` for *nix and `%PATH%` for Windows.


#### A: Configuring your $PATH on *nix

This is almost always achievable on a per-user basis through the user's
`.profile` (Users running Bash may prefer `.bash_profile`). The file is located
in your home directory.  Open the file and add the following line, assuming the
`li3` wrapper exists in `/path/to/libraries/lithium/console`.

{{{
export PATH+=:/path/to/libraries/lithium/console
}}}

Once you've followed these steps, save your modified the file and reload your environment settings
by sourcing the modified profile through the following command.

{{{
$ source ~/.profile
}}}

If you are unable, or don't wish, to modify your `$PATH` there are two other
techniques you may use to make the wrapper available as `li3`.  You can either
symlink the wrapper into one of the paths found in the `$PATH` environment
variable or create an alias. If you choose an alias, you can make it permanent
by adding the alias command to the `.bashrc` file in your home directory.

{{{
$ cd /path/to/a/directory/in/your/path
$ ln -s /path/to/libraries/lithium/console .
}}}

{{{
alias li3='/path/to/lithium/libraries/lithium/console/li3'
}}}

#### B: Configuring your %PATH% on Windows

Please note that if you're on Windows you must also add the PHP directory to
the `%PATH%` environment variable. As we are going to edit that variable for adding
the location of the `li3.bat` wrapper anyway, we can kill two birds with one stone.

 - Open _System_ from within the _Control Panel_.
 - Open the _Advanced_ tab.
 - Clicking the _Environment Variables_ button open a dialog where you can edit the variables.
 - Double click the _PATH_ entry in order to edit it.
 - Add `;C:\path\to\php;C:\path\to\libraries\lithium\console` to the end of the value.

#### Finishing up

Now that you've made the wrapper available as `li3` or `li3.bat` respectively,
you should be able to use it from the command-line simply by executing `li3` or
`li3.bat`. Invoking the wrapper like that (without arguments) should give you a
list of available commands.

{{{
$ li3
$ li3.bat
}}}

### Built-in commands

Using the commands which come with lithium is easy. Invoke the wrapper without
any arguments to get a list of all available commands. Get a description about
each command and the options and arguments it accepts or may require by using
the `help` command.

{{{
$ li3 help
$ li3 help create
$ li3 help g11n
}}}

### Creating custom commands

Creating your own commands is very easy. A few fundamentals:

- All commands inherit from `lithium\console\Command`.
- Commands are normally placed in your application or library's `extensions/command` directory.

Here's an example command:

{{{
<?php

namespace app\extensions\command;

class HelloWorld extends \lithium\console\Command {

	public function run() {
		$this->header('Welcome to the Hello World command!');
		$this->out('Hello, World!');
	}
}

?>
}}}

If you would like to try this command, create an application or use an existing
application, place the command into the application's `extensions/commands`
directory and save it as `HelloWorld.php`. After doing so open a shell and
change directory to your application's directory and run the following command:

{{{
$ li3 hello_world
}}}

Although it's probably obvious, when this command runs it will output a nice
header with the text `Welcome to the Hello World command!` and some regular
text `Hello, World!` after it.

The public method `run()` is called on your command instance every time your
command has been requested to run. From this method you can add your own command
logic.

#### Parsing options and arguments

Parsing options and arguments to commands should be simple. In fact, the
parsing is already done for you.

Short and long (GNU-style) options in the form of `-f`, `--foo`, `--foo-bar` and `--foo=bar`
are automatically parsed and exposed to your command instance through its
properties. XF68-style long options (i.e. `-foo`) are not supported by default
but support can be added by extending the console router.

Arguments are passed directly to the invoked method.

Let's look at an example, going back to the `hello_world` command from earlier:

{{{
<?php

namespace app\extensions\command;

class HelloWorld extends \lithium\console\Command {

	public $recipient;

	public function run() {
		$this->header('Welcome to the Hello World command!');
		$this->out('Hello, ' . ($this->recipient ?: 'World') . '!');
	}
}

?>
}}}

Notice the additional property `$recipient`? Great! Now when `--recipient` is
passed to the `hello_world` command, the recipient property on your command
instance will be set to whatever was passed into the command at runtime.

Try it out with the following command:

{{{
$ li3 hello_world --recipient=AwesomeGuy
}}}

You should get a special greeting from our good old `hello_world` command.
