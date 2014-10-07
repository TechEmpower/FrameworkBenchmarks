<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\console\command;

use Phar;
use Exception;
use RuntimeException;
use lithium\core\Libraries;
use lithium\util\String;
use lithium\util\Inflector;

/**
 * The Library command is used to archive and extract Phar::GZ archives. Requires zlib extension.
 * In addition, communicate with the a given server to add plugins and extensions to the
 * current application. Push archived plugins to the server.
 *
 */
class Library extends \lithium\console\Command {

	/**
	 * Absolute path to config file.
	 *
	 * @var string
	 */
	public $conf = null;

	/**
	 * Path to where plugins will be installed. Relative to current working directory.
	 *
	 * @var string
	 */
	public $path = null;

	/**
	 * Server host to query for plugins.
	 *
	 * @var string
	 */
	public $server = 'lab.lithify.me';

	/**
	 * The port for the server.
	 *
	 * @var string
	 */
	public $port = 80;

	/**
	 * The username for the server authentication.
	 *
	 * @var string
	 */
	public $username = '';

	/**
	 * The password for corresponding username.
	 *
	 * @var string
	 */
	public $password = '';

	/**
	 * @see `force`
	 * @var boolean
	 */
	public $f = false;

	/**
	 * Force operation to complete. Typically used for overwriting files.
	 *
	 * @var string
	 */
	public $force = false;

	/**
	 * Filter used for including files in archive.
	 *
	 * @var string
	 */
	public $filter = '/\.(php|htaccess|jpg|png|gif|css|js|ico|json|ini)|(empty)$/';

	/**
	 * Namespace used for newly extracted libraries.
	 * Will default to the basename of the directory
	 * the library is being extracted to.
	 *
	 * @var string
	 */
	public $namespace = null;

	/**
	 * When extracting a library, custom replacements
	 * can be made on the extracted files that
	 * are defined in this json file.
	 *
	 * @var string
	 */
	public $replacementsFile = '_replacements.json';

	/**
	 * The path to use for the `LITHIUM_LIBRARY_PATH`
	 * in extracted templates.  It defaults to the
	 * current value of the `LITHIUM_LIBRARY_PATH`
	 * constant.  If `LITHIUM_LIBRARY_PATH` is not the same
	 * as `dirname(LITHIUM_APP_PATH) . '/libraries'` then
	 * the value of `LITHIUM_LIBRARY_PATH` will be hard-coded
	 * to the `config/bootstrap/libraries.php` file in the
	 * extracted library.  If you want it to use a custom
	 * value, then pass it to this option.  For example,
	 * if you keep your apps in the same directory as your
	 * libraries, you could set it to `dirname(LITHIUM_APP_PATH)`
	 *
	 * @var string
	 */
	public $lithiumLibraryPath = LITHIUM_LIBRARY_PATH;

	/**
	 * Holds settings from conf file
	 *
	 * @var array
	 */
	protected $_settings = array();

	/**
	 * some classes
	 *
	 * @var array
	 */
	protected $_classes = array(
		'service' => 'lithium\net\http\Service',
		'response' => 'lithium\console\Response'
	);

	/**
	 * Auto configuration properties.
	 *
	 * @var array
	 */
	protected $_autoConfig = array(
		'classes' => 'merge', 'env', 'detectors' => 'merge', 'base', 'type', 'stream'
	);

	/**
	 * Initialize _settings from `--conf`.
	 *
	 * Throws an exception if the command is  initialized without a request object
	 * which is needed by `_toPath()` in order to determine the current working directory.
	 * This most often happens if the command is inspected using the `ReflectionClass`.
	 *
	 * @return void
	 */
	protected function _init() {
		parent::_init();
		if ($this->server) {
			$this->_settings['servers'][$this->server] = true;
		}
		if (file_exists($this->conf)) {
			$this->_settings += (array) json_decode($this->conf, true);
		}
		$this->path = $this->_toPath($this->path ?: 'libraries');
		$this->force = $this->f ? $this->f : $this->force;
	}

	/**
	 * Add configuration and write data in json format.
	 *
	 * @param string $key (server)
	 * @param string $value value of key
	 * @param boolean|string $options [optional]
	 * @return mixed Returns all settings if `$key` and `$value` aren't set. The only option for
	 *         `$key` right now is 'server'. Returns the bytes written to the configuration file.
	 */
	public function config($key = null, $value = null, $options = true) {
		if (empty($key) || empty($value)) {
			return $this->_settings;
		}
		switch ($key) {
			case 'server':
				$this->_settings['servers'][$value] = $options;
			break;
		}
		return file_put_contents($this->conf, json_encode($this->_settings));
	}

	/**
	 * Extract an archive into a path. If one param exists, the app.phar.gz template will be used.
	 * If both parameters exist, then the first will be the template archive and the second will be
	 * the name of the extracted archive
	 *
	 * - `li3 library extract myapp` : uses command/create/template/app.phar.gz
	 * - `li3 library extract another_archive myapp` : uses
	 *       command/create/template/another_archive.phar.gz
	 * - `li3 library extract plugin li3_plugin` : uses command/create/template/plugin.phar.gz
	 * - `li3 library extract /full/path/to/a.phar.gz myapp` : paths that begin with a '/'
	 *       can extract from archives outside of the default command/create/template/
	 *       location
	 *
	 * @param string $name if only param, command/create/template/app.phar.gz extracted to $name
	 *     otherwise, the template name or full path to extract `from` phar.gz.
	 * @param string $result if exists $name is extracted to $result
	 * @return boolean
	 */
	public function extract($name = 'new', $result = null) {
		$from = 'app';
		$to = $name;

		if ($result) {
			$from = $name;
			$to = $result;
		}

		$to = $this->_toPath($to);

		if ($from[0] !== '/') {
			$from = Libraries::locate('command.create.template', $from, array(
				'filter' => false, 'type' => 'file', 'suffix' => '.phar.gz'
			));
			if (!$from || is_array($from)) {
				return false;
			}
		}

		if (file_exists($from)) {
			try {
				$archive = new Phar($from);
			} catch (Exception $e) {
				$this->error($e->getMessage());
				return false;
			}

			if ($archive->extractTo($to)) {
				$this->out(basename($to) . " created in " . dirname($to) . " from {$from}");

				if (empty($this->namespace)) {
					$this->namespace = Inflector::underscore(basename($to));
				}

				$replacements = $this->_findReplacements($to);
				return $this->_replaceAfterExtract($to, compact('namespace', 'replacements'));
			}
		}
		$this->error("Could not extract {$to} from {$from}");
		return false;
	}

	/**
	 * Helper method for `console\command\Library::extract()` to gather
	 * replacements to perform on the newly extracted files
	 *
	 * It looks for a json file specified by `$this->replacementsFile`
	 * which defaults to _replacements.json.
	 *
	 * Running eval on a php file to get the `$replacements`
	 * would be more flexible than using json, but definitely much more of a
	 * security hole if the library is not trusted.
	 *
	 * @param string $base File path to the extracted library
	 * @return array A multi-dimensional array.  Keys on the top level
	 *     are filenames or glob-style paths.  Those hold an array
	 *     with keys being the search param and values being the
	 *     replacement values
	 */
	protected function _findReplacements($base = null) {
		$replacements = null;
		if (file_exists($base . '/' . $this->replacementsFile)) {
			$replacementsFilename = $base . '/' . $this->replacementsFile;
			$replacements = json_decode(file_get_contents($replacementsFilename), true);
			if ($replacements !== false) {
				unlink($base . '/' . $this->replacementsFile);
			}
		}
		return $replacements;
	}

	/**
	 * Helper method for `console\command\Library::extract()` to perform after-extract string
	 * replacements.
	 *
	 * In the current implementation, it only sets the correct `LITHIUM_LIBRARY_PATH` when the
	 * app.phar.gz archive was extracted. If you get any errors, please make sure that the console
	 * script has read and write permissions to the extracted directory.
	 *
	 * @param string $extracted contains the path to the extracted archive.
	 * @param array $options Valid options are:
	 *     - `'replacements'`: an array of string replacements indexed by filename.
	 *       It's also possible to use glob-style wildcards in the filename such
	 *       as `*` or `*.php` or `resources/g11n/*`.  If the filename starts
	 *       with `*`, then that filename pattern will be recursively found
	 *       in every sub-directory.  Additionally, each replacement can
	 *       use `String::insert()` style strings that will be replaced
	 *       with the data in the `data` option.
	 *     - `'data'`: an array with data that will be used to replace
	 *       `String::insert`-style placeholders in the `replacements` option.
	 *       By default, this includes 'namespace' and 'library' which are
	 *       both set to the extracted library's namespace.
	 * @return boolean
	 */
	protected function _replaceAfterExtract($extracted, $options = array()) {
		$namespace = $this->namespace;
		$library = $namespace;
		$data = compact('namespace', 'library');
		$replacements = array();
		extract($options);

		if (empty($replacements)) {
			$replacements = array(
				'config/bootstrap/libraries.php' => array(
					"Libraries::add('app'" => "Libraries::add('{:namespace}'"
				),
				'*.php' => array(
					"namespace app\\" => "namespace {:namespace}\\"
				)
			);
		}

		if (dirname(LITHIUM_APP_PATH) . '/libraries' !== $this->lithiumLibraryPath) {
			$pathinfo = pathinfo($this->lithiumLibraryPath);
			if ($pathinfo['dirname'] !== '.') {
				$this->lithiumLibraryPath = "'" . $this->lithiumLibraryPath . "'";
			}

			$search = 'define(\'LITHIUM_LIBRARY_PATH\', ';
			$search .= 'dirname(LITHIUM_APP_PATH) . \'/libraries\');';
			$replace = 'define(\'LITHIUM_LIBRARY_PATH\', ';
			$replace .= $this->lithiumLibraryPath . ');';

			if (!isset($replacements['config/bootstrap/libraries.php'])) {
				$replacements['config/bootstrap/libraries.php'] = array();
			}
			$replacements['config/bootstrap/libraries.php'][$search] = $replace;
		}

		foreach ($replacements as $filename => $definitions) {
			foreach ($definitions as $search => $replace) {
				unset($definitions[$search]);
				$search = String::insert($search, $data);
				$replace = String::insert($replace, $data);
				$definitions[$search] = $replace;
			}
			$paths = $this->_wildcardPaths($filename, $extracted);
			foreach ($paths as $filepath) {
				if (file_exists($filepath)) {
					$content = file_get_contents($filepath);
					if ($content === '') {
						continue;
					}
					$content = str_replace(
						array_keys($definitions),
						array_values($definitions),
						$content
					);
					if (!file_put_contents($filepath, $content)) {
						$this->error("Could not replace content in {$filepath}");
						return false;
					}
				}
			}
		}
		return true;
	}

	/**
	 * Utility function that will return an array of
	 * file paths relative to the `$base` path that
	 * are found using a glob-style asterisk wildcards
	 * such as `*` or `*.php` or `resources/g11n/*`.  If the path starts
	 * with `*`, then that filename pattern will be recursively found
	 * in every sub-directory.
	 *
	 * @param string $path
	 * @param string $base Base directory to search for matching files
	 * @return array
	 */
	protected function _wildcardPaths($path, $base = '') {
		if (strpos($path, '*') === false) {
			return array($base . '/' . $path);
		}
		if ($path[0] === '*') {
			$paths = array();
			$dirs = array($base);
			while (!empty($dirs)) {
				$dir = array_shift($dirs);
				$paths = array_merge($paths, glob($dir . '/' . $path));
				$dirs = array_merge(
					$dirs,
					array_filter(glob($dir . '/*'), function($path) {
						return is_dir($path);
					})
				);
			}
		} else {
			$paths = array_filter(glob($base . '/' . $path), function($path) {
				$basename = basename($path);
				return $basename !== '.' && $basename !== '..';
			});
		}
		return $paths;
	}

	/**
	 * Create the Phar::GZ archive from a given directory. If no params, the current working
	 * directory is archived with the name of that directory. If one param, the current working
	 * directory will be archive with the name provided. If both params, the first is the
	 * name or path to the library to archive and the second is the name of the resulting archive
	 *
	 * - `li3 library archive my_archive` : archives current working directory to my_archive.phar.gz
	 * - `li3 library archive myapp my_archive` : archives 'myapp' to 'my_archive.phar.gz'
	 *
	 * @param string $name if only param, the archive name for the current working directory
	 *     otherwise, The library name or path to the directory to compress.
	 * @param string $result if exists, The name of the resulting archive
	 * @return boolean
	 */
	public function archive($name = null, $result = null) {
		if (ini_get('phar.readonly') === '1') {
			throw new RuntimeException('Set `phar.readonly` to `0` in `php.ini`.');
		}
		$from = $name;
		$to = $name;

		if ($result) {
			$from = $name;
			$to = $result;
		}
		$path = $this->_toPath($to);

		if (file_exists("{$path}.phar")) {
			if (!$this->force) {
				$this->error(basename($path) . ".phar already exists in " . dirname($path));
				return false;
			}
			Phar::unlinkArchive("{$path}.phar");
		}
		try {
			$archive = new Phar("{$path}.phar");
		} catch (Exception $e) {
			$this->error($e->getMessage());
			return false;
		}
		$result = null;
		$from = $this->_toPath($from);

		if (is_dir($from)) {
			$result = (boolean) $archive->buildFromDirectory($from, $this->filter);
		}
		if (file_exists("{$path}.phar.gz")) {
			if (!$this->force) {
				$this->error(basename($path) . ".phar.gz already exists in " . dirname($path));
				return false;
			}
			Phar::unlinkArchive("{$path}.phar.gz");
		}
		if ($result) {
			$archive->compress(Phar::GZ);
			$this->out(basename($path) . ".phar.gz created in " . dirname($path) . " from {$from}");
			return true;
		}
		$this->error("Could not create archive from {$from}");
		return false;
	}


	/**
	 * List all the plugins and extensions available on the server.
	 *
	 * @param string $type plugins|extensions
	 */
	public function find($type = 'plugins') {
		$results = array();

		foreach ($this->_settings['servers'] as $server => $enabled) {
			if (!$enabled) {
				continue;
			}
			$service = $this->_instance('service', array(
				'host' => $server, 'port' => $this->port
			));
			$results[$server] = json_decode($service->get("lab/{$type}.json"));

			if (empty($results[$server])) {
				$this->out("No {$type} at {$server}");
				continue;
			}
			foreach ((array) $results[$server] as $data) {
				$name = isset($data->class) ? $data->class : $data->name;
				$header = "{$server} > {$name}";
				$out = array(
					"{$data->summary}",
					"Version: {$data->version}",
					"Created: {$data->created}"
				);
				$this->header($header);
				$this->out(array_filter($out));
			}
		}
	}

	/**
	 * Install plugins or extensions to the current application.
	 * For plugins, the install commands specified in the formula is run.
	 *
	 * @param string $name name of plugin to add
	 * @return boolean
	 */
	public function install($name = null) {
		$results = array();

		foreach ($this->_settings['servers'] as $server => $enabled) {
			if (!$enabled) {
				continue;
			}
			$service = $this->_instance('service', array('host' => $server, 'port' => $this->port));

			if ($plugin = json_decode($service->get("lab/{$name}.json"))) {
				break;
			}
		}
		if (empty($plugin->sources)) {
			$this->error("{$name} not found.");
			return false;
		}
		$hasGit = function () {
			return (strpos(shell_exec('git --version'), 'git version') !== false);
		};
		foreach ((array) $plugin->sources as $source) {
			if (strpos($source, 'phar.gz') !== false && file_exists($source)) {
				$written = file_put_contents(
					"{$this->path}/{$plugin->name}.phar.gz", file_get_contents($source)
				);
				if (!$written) {
					$this->error("{$plugin->name}.phar.gz could not be saved");
					return false;
				}
				$this->out("{$plugin->name}.phar.gz saved to {$this->path}");

				try {
					$archive = new Phar("{$this->path}/{$plugin->name}.phar.gz");

					if ($archive->extractTo("{$this->path}/{$plugin->name}")) {
						$this->out("{$plugin->name} installed to {$this->path}/{$plugin->name}");
						$this->out("Remember to update the bootstrap.");
						return true;
					}
				} catch (Exception $e) {
					$this->error($e->getMessage());
				}
			}
			$url = parse_url($source);

			if (!empty($url['scheme']) && $url['scheme'] === 'git' && $hasGit()) {
				$cmd = "cd {$this->path} && git clone --quiet {$source} {$plugin->name}";
				$result = shell_exec($cmd);

				if (is_dir("{$this->path}/{$plugin->name}")) {
					$this->out("{$plugin->name} installed to {$this->path}/{$plugin->name}");
					$this->out("Remember to update your bootstrap.");
					return true;
				}
			}
		}
		$this->out("{$plugin->name} not installed.");
		return false;
	}

	/**
	 * Create a formula for the given library name
	 *
	 * @param string $name the library name or full path to the plugin
	 * @return boolean
	 */
	public function formulate($name = null) {
		if (!$name) {
			$name = $this->in("please supply a name");
		}
		$result = false;
		$path = $this->_toPath($name);
		$name = basename($path);
		$formula = "{$path}/config/{$name}.json";

		$data = array();

		if (file_exists($formula)) {
			$data = json_decode(file_get_contents($formula), true);
		}
		if (empty($data['version'])) {
			$data['version'] = $this->in("please supply a version");
		}
		if (empty($data['summary'])) {
			$data['summary'] = $this->in("please supply a summary");
		}
		if (file_exists($path) && !file_exists($formula)) {
			$defaults = array(
				'name' => $name, 'version' => '0.1',
				'summary' => "a plugin called {$name}",
				'maintainers' => array(array(
					'name' => '', 'email' => '', 'website' => ''
				)),
				'sources' => array("http://{$this->server}/lab/download/{$name}.phar.gz"),
				'commands' => array(
					'install' => array(), 'update' => array(), 'remove' => array()
				),
				'requires' => array()
			);
			$data += $defaults;

			if (!is_dir(dirname($formula)) && !mkdir(dirname($formula), 0755, true)) {
				$this->error("Formula for {$name} not created in {$path}");
				return false;
			}
		}
		if (is_dir(dirname($formula)) && file_put_contents($formula, json_encode($data))) {
			$this->out("Formula for {$name} created in {$path}.");
			return true;
		}
		$this->error("Formula for {$name} not created in {$path}");
		return false;
	}

	/**
	 * Send a plugin archive to the server. The plugin must have a formula.
	 *
	 * @param string $name the library name or full path to the archive to send
	 * @return void
	 */
	public function push($name = null) {
		if (!$name) {
			$name = $this->in("please supply a name");
		}
		$path = $this->_toPath($name);
		$name = basename($name);
		$file = "{$path}.phar.gz";

		if (!file_exists("phar://{$file}/config/{$name}.json")) {
			$this->error(array(
				"The formula for {$name} is missing.", "Run li3 library formulate {$name}"
			));
			return false;
		}
		$formula = json_decode(file_get_contents("phar://{$file}/config/{$name}.json"));
		$isValid = (
			!empty($formula->name) && !empty($formula->version)
			&& !empty($formula->summary) && !empty($formula->sources)
		);
		if (!$isValid) {
			$this->error(array(
				"The formula for {$name} is not valid.", "Run li3 library formulate {$name}"
			));
			return false;
		}
		if (file_exists($file)) {
			$service = $this->_instance('service', array(
				'host' => $this->server, 'port' => $this->port,
				'auth' => 'Basic', 'username' => $this->username, 'password' => $this->password
			));
			$boundary = md5(date('r', time()));
			$headers = array("Content-Type: multipart/form-data; boundary={$boundary}");
			$name = basename($file);
			$data = join("\r\n", array(
				"--{$boundary}",
				"Content-Disposition: form-data; name=\"phar\"; filename=\"{$name}\"",
				"Content-Type: application/phar", "",
				base64_encode(file_get_contents($file)),
				"--{$boundary}--"
			));
			$result = json_decode($service->post(
				'/lab/server/receive', $data, compact('headers')
			));

			if ($service->last->response->status['code'] == 201) {
				$this->out(array(
					"{$result->name} added to {$this->server}.",
					"See http://{$this->server}/lab/plugins/view/{$result->id}"
				));
				return $result;
			}
			if (!empty($result->error)) {
				$this->error($result->error);
				return false;
			}
			$this->error((array) $result);
			return false;
		}
		$this->error(array("{$file} does not exist.", "Run li3 library archive {$name}"));
		return false;
	}

	/**
	 * Update installed plugins. For plugins, runs update commands specified in Formula.
	 *
	 * @todo implement
	 * @return void
	 */
	public function update() {
		$this->error('Please implement me');
	}

	/**
	 * Take a name and return the path.
	 *
	 * If the name already appears to be a path, it is returned directly. Otherwise, the
	 * `Library` class is used to find the associated path.
	 *
	 * @param string $name
	 * @return string
	 */
	protected function _toPath($name = null) {
		$pathinfo = pathinfo($name);
		if ($name && $pathinfo['dirname'] !== '.') {
			return $name;
		}

		$library = Libraries::get($name);

		if (!empty($library['path'])) {
			return $library['path'];
		}

		$path = $this->request->env('working');
		return (!empty($name)) ? "{$path}/{$name}" : $path;
	}
}

?>