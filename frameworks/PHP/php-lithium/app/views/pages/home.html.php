<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

use lithium\core\Libraries;
use lithium\core\Environment;
use lithium\data\Connections;

$this->title('Home');

$self = $this;

$notify = function($status, $message, $solution = null) {
	$html  = "<div class=\"test-result test-result-{$status}\">{$message}</div>";
	$html .= "<div class=\"test-result solution\">{$solution}</div>";
	return $html;
};

$support = function($classes) {
	$result = '<ul class="indicated">';

	foreach ($classes as $class => $enabled) {
		$name = substr($class, strrpos($class, '\\') + 1);
		$url = 'http://lithify.me/docs/' . str_replace('\\', '/', $class);
		$class = $enabled ? 'enabled' : 'disabled';
		$title = $enabled ? "Adapter `{$name}` is enabled." : "Adapter `{$name}` is disabled.";

		$result .= "<li><a href=\"{$url}\" title=\"{$title}\" class=\"{$class}\">{$name}</a></li>";
	}
	$result .= '</ul>';

	return $result;
};

$compiled = function($flag) {
	ob_start();
	phpinfo(INFO_GENERAL);
	return strpos(ob_get_clean(), $flag) !== false;
};

$checks = array(
	'resourcesWritable' => function() use ($notify) {
		if (is_writable($path = Libraries::get(true, 'resources'))) {
			return $notify('success', 'Resources directory is writable');
		}
		$path = str_replace(dirname(LITHIUM_APP_PATH) . '/', null, $path);
		$solution = null;

		if (strtoupper(substr(PHP_OS, 0, 3)) !== 'WIN') {
			$solution  = 'To fix this, run the following from the command line: ';
			$solution .= "<code>$ chmod -R 0777 {$path}</code>.";
		} else {
			$path = realpath($path);
			$solution  = 'To fix this, give <code>modify</code> rights to the user ';
			$solution .= "<code>Everyone</code> on directory <code>{$path}</code>.";
		}
		return $notify(
			'fail',
			'Your resource path is not writeable',
			$solution
		);
	},
	'magicQuotes' => function() use ($notify) {
		if (!get_magic_quotes_gpc()) {
			return;
		}
		return $notify(
			'fail',
			'Magic quotes are enabled in your PHP configuration',
			'Please set <code>magic_quotes_gpc = Off</code> in your <code>php.ini</code> settings.'
		);
	},
	'registerGlobals' => function() use ($notify) {
		if (!ini_get('register_globals')) {
			return;
		}
		return $notify(
			'fail',
			'Register globals is enabled in your PHP configuration',
			'Please set <code>register_globals = Off</code> in your <code>php.ini</code> settings.'
		);
	},
	'curlwrappers' => function() use ($notify, $compiled) {
		if (!$compiled('with-curlwrappers')) {
			return;
		}
		return $notify(
			'fail',
			'Curlwrappers are enabled, some things might not work as expected.',
			"This is an expiremental and usually broken feature of PHP.
			Please recompile your PHP binary without using the <code>--with-curlwrappers</code>
			flag or use a precompiled binary that was compiled without the flag."
		);
	},
	'shortOpenTag' => function() use ($notify, $compiled) {
		if (!ini_get('short_open_tag')) {
			return;
		}
		return $notify(
			'notice',
			'Short open tags are enabled, you may want to disable them.',
			"It is recommended to not rely on this option being enabled.
			To increase the portability of your code disable this option by setting
			<code>short_open_tag = Off</code> in your <code>php.ini</code>."
		);
	},
	'dbSupport' => function() use ($notify, $support) {
		$paths = array('data.source', 'adapter.data.source.database', 'adapter.data.source.http');
		$list = array();

		foreach ($paths as $path) {
			$list = array_merge($list, Libraries::locate($path, null, array('recursive' => false)));
		}
		$list = array_filter($list, function($class) { return method_exists($class, 'enabled'); });
		$map = array_combine($list, array_map(function($c) { return $c::enabled(); }, $list));

		return $notify('notice', 'Database support', $support($map));
	},
	'cacheSupport' => function() use ($notify, $support) {
		$list = Libraries::locate('adapter.storage.cache', null, array('recursive' => false));
		$list = array_filter($list, function($class) { return method_exists($class, 'enabled'); });
		$map = array_combine($list, array_map(function($c) { return $c::enabled(); }, $list));

		return $notify('notice', 'Cache support', $support($map));
	},
	'database' => function() use ($notify) {
		if ($config = Connections::config()) {
			return $notify('success', 'Database connection(s) configured');
		}
		return $notify(
			'notice',
			'No database connection defined',
			"To create a database connection:
			<ol>
				<li>Edit the file <code>config/bootstrap.php</code>.</li>
				<li>
					Uncomment the line having
					<code>require __DIR__ . '/bootstrap/connections.php';</code>.
				</li>
				<li>Edit the file <code>config/bootstrap/connections.php</code>.</li>
			</ol>"
		);
	},
	'change' => function() use ($notify, $self) {
		$template = $self->html->link('template', 'http://lithify.me/docs/lithium/template');

		return $notify(
			'notice',
			"You're using the application's default home page",
			"To change this {$template}, edit the file
			<code>views/pages/home.html.php</code>.
			To change the layout,
			(that is what's wrapping content)
			edit the file <code>views/layouts/default.html.php</code>."
		);
	},
	'routing' => function() use ($notify, $self) {
		$routing = $self->html->link('routing', 'http://lithify.me/docs/lithium/net/http/Router');

		return $notify(
			'notice',
			'Use custom routing',
			"Routes allow you to map custom URLs to your application code. To change the
			{$routing}, edit the file <code>config/routes.php</code>."
		);
	},
	'tests' => function() use ($notify, $self) {
		if (Environment::is('production')) {
			$docsLink = $self->html->link(
				'the documentation',
				'http://lithify.me/docs/lithium/core/Environment::is()'
			);

			return $notify(
				'fail',
				"Can't run tests",
				"<p>Lithium's default environment detection rules have determined that you are
				running in production mode. Therefore, you will not be able to run tests from the
				web interface. You can do any of the following to remedy this:</p>
				<ul>
					<li>Run this application locally</li>
					<li>Run tests from the console, using the <code>li3 test</code> command</li>
					<li>
						Implementing custom environment detection rules;
						see {$docsLink} for examples
					</li>
				</ul>"
			);
		}
		$tests = $self->html->link('run all tests', array(
			'controller' => 'lithium\test\Controller',
			'args' => 'all'
		));
		$dashboard = $self->html->link('test dashboard', array(
			'controller' => 'lithium\test\Controller'
		));
		$ticket = $self->html->link(
			'file a ticket', 'https://github.com/UnionOfRAD/lithium/issues'
		);

		return $notify(
			'notice',
			'Run the tests',
			"Check the builtin {$dashboard} or {$tests} now to ensure Lithium
			is working as expected. Do not hesitate to {$ticket} in case a test fails."
		);
	}
);

?>

<?php foreach ($checks as $check): ?>
	<?php echo $check(); ?>
<?php endforeach; ?>

<ul class="additional-resources">
	<li>
		<div class="test-result test-result-notice">Getting started</div>
		<div class="test-result solution">
			<?php echo $this->html->link(
				'Quickstart', 'http://lithify.me/docs/manual/quickstart'
			); ?> is a guide for PHP users who are looking to get a good idea of what Lithium can
			do. The guide is part of the official Lithium manual, <?php echo $this->html->link(
				'The Definitive Guide', 'http://lithify.me/docs/manual'
			); ?>.
		</div>
	</li>
	<li>
		<div class="test-result test-result-notice">Learn more</div>
		<div class="test-result solution">
			The
			<?php echo $this->html->link('API documentation', 'http://lithify.me/docs/lithium'); ?>
			has all the implementation details you've been looking for.
		</div>
	</li>
	<li>
		Chat with other Lithium users and the team developing Lithium.
		For <em>general support</em> hop on the
		<?php echo $this->html->link('#li3 channel', 'irc://irc.freenode.net/#li3'); ?>
		or read the
		<?php echo $this->html->link('logs', 'http://lithify.me/bot/logs/li3'); ?>.
		For <em>core discussions</em> join us in the
		<?php echo $this->html->link('#li3-core channel', 'irc://irc.freenode.net/#li3-core'); ?>
		or read the
		<?php echo $this->html->link('logs', 'http://lithify.me/bot/logs/li3-core'); ?>.
	</li>
	<li>
		Browse the Lithium
		<?php echo $this->html->link('Repository', 'https://github.com/UnionOfRAD/lithium'); ?>
		or read the
		<?php echo $this->html->link('Wiki', 'https://github.com/UnionOfRAD/lithium/wiki'); ?>.
	</li>
</ul>