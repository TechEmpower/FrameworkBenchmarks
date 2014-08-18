<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\template\view;

use lithium\core\Libraries;
use lithium\template\TemplateException;

/**
 * The template compiler is a simple string replacement engine which allows PHP templates to be
 * overridden with custom syntax. The default process rules allow PHP templates using short-echo
 * syntax (`<?=`) to be rewritten to full PHP tags which automatically escape their output.
 *
 * It is possible to create your own template compiler and have the chosen `View` adapter use that
 * instead. Please see the documentation on the dynamic dependencies of the adapter in question
 * to know more about how this can be achieved.
 *
 * @see lithium\template\View
 * @see lithium\template\view\adapter
 */
class Compiler extends \lithium\core\StaticObject {

	/**
	 * The list of syntax replacements to apply to compiled templates.
	 *
	 * Key/value pairs of regular expressions. The keys are the regexes, and the values are the
	 * resulting expressions along with any capture groups that may have been used in the
	 * corresponding regexes.
	 *
	 * @var array
	 */
	protected static $_processors = array(
		'/\<\?=\s*\$this->(.+?)\s*;?\s*\?>/msx' => '<?php echo $this->$1; ?>',
		'/\<\?=\s*(\$h\(.+?)\s*;?\s*\?>/msx' => '<?php echo $1; ?>',
		'/\<\?=\s*(.+?)\s*;?\s*\?>/msx' => '<?php echo $h($1); ?>'
	);

	/**
	 * Compiles a template and writes it to a cache file, which is used for inclusion.
	 *
	 * @param string $file The full path to the template that will be compiled.
	 * @param array $options Options for compilation include:
	 *        - `path`: Path where the compiled template should be written.
	 *        - `fallback`: Boolean indicating that if the compilation failed for some
	 *                      reason (e.g. `path` is not writable), that the compiled template
	 *                      should still be returned and no exception be thrown.
	 * @return string The compiled template.
	 */
	public static function template($file, array $options = array()) {
		$cachePath = Libraries::get(true, 'resources') . '/tmp/cache/templates';
		$defaults = array('path' => $cachePath, 'fallback' => false);
		$options += $defaults;

		$stats = stat($file);
		$dir = dirname($file);
		$oname = basename(dirname($dir)) . '_' . basename($dir) . '_' . basename($file, '.php');
		$template = "template_{$oname}_{$stats['ino']}_{$stats['mtime']}_{$stats['size']}.php";
		$template = "{$options['path']}/{$template}";

		if (file_exists($template)) {
			return $template;
		}
		$compiled = static::compile(file_get_contents($file));

		if (is_writable($cachePath) && file_put_contents($template, $compiled) !== false) {
			foreach (glob("{$options['path']}/template_{$oname}_*.php") as $expired) {
				if ($expired !== $template) {
					unlink($expired);
				}
			}
			return $template;
		}
		if ($options['fallback']) {
			return $file;
		}
		throw new TemplateException("Could not write compiled template `{$template}` to cache.");
	}

	/**
	 * Preprocess the passed `$string` (usually a PHP template) for syntax replacements
	 * using sets of regular expressions.
	 *
	 * @see lithium\template\view\Compiler::$_processors
	 * @param string $string The string to be preprocessed.
	 * @return string Processed string.
	 */
	public static function compile($string) {
		$patterns = static::$_processors;
		return preg_replace(array_keys($patterns), array_values($patterns), $string);
	}
}

?>