<?php
/**
 * Fuel is a fast, lightweight, community driven PHP5 framework.
 *
 * @package    Fuel
 * @version    1.5
 * @author     Fuel Development Team
 * @license    MIT License
 * @copyright  2010 - 2013 Fuel Development Team
 * @link       http://fuelphp.com
 */

namespace Oil;

/**
 * Oil\Console Class
 *
 * @package		Fuel
 * @subpackage	Oil
 * @category	Core
 * @author		Phil Sturgeon
 */

class Console
{

	public function __construct()
	{
		error_reporting(E_ALL | E_STRICT);

		ini_set("error_log", NULL);
		ini_set("log_errors", 1);
		ini_set("html_errors", 0);
		ini_set("display_errors", 0);

		while (ob_get_level ())
		{
			 ob_end_clean();
		}

		ob_implicit_flush(true);

		// And, go!
		self::main();
	}

	private function main()
	{
		\Cli::write(sprintf(
			'Fuel %s - PHP %s (%s) (%s) [%s]',
			\Fuel::VERSION,
			phpversion(),
			php_sapi_name(),
			self::build_date(),
			PHP_OS
		));

		// Loop until they break it
		while (TRUE)
		{
			if (\Cli::$readline_support)
			{
				readline_completion_function(array(__CLASS__, 'tab_complete'));
			}

			if ( ! $__line = rtrim(trim(trim(\Cli::input('>>> ')), PHP_EOL), ';'))
			{
				continue;
			}

			if ($__line == 'quit')
			{
				break;
			}

			// Add this line to history
			//$this->history[] = array_slice($this->history, 0, -99) + array($line);
			if (\Cli::$readline_support)
			{
				readline_add_history($__line);
			}

			if (self::is_immediate($__line))
			{
				$__line = "return ($__line)";
			}

			ob_start();

			// Unset the previous line and execute the new one
			$random_ret = \Str::random();
			try
			{
				$ret = eval("unset(\$__line); $__line;");
			}
			catch(\Exception $e)
			{
				$ret = $random_ret;
				$__line = $e->getMessage();
			}

			// Error was returned
			if ($ret === $random_ret)
			{
				\Cli::error('Parse Error - ' . $__line);
				\Cli::beep();
			}

			if (ob_get_length() == 0)
			{
				if (is_bool($ret))
				{
					echo $ret ? 'true' : 'false';
				}
				elseif (is_string($ret))
				{
					echo addcslashes($ret, "\0..\37\177..\377");
				}
				elseif ( ! is_null($ret))
				{
					var_export($ret);
				}
			}

			unset($ret);
			$out = ob_get_contents();
			ob_end_clean();

			if ((strlen($out) > 0) && (substr($out, -1) != PHP_EOL))
			{
				$out .= PHP_EOL;
			}

			echo $out;
			unset($out);
		}
	}

	private static function is_immediate($line)
	{
		$skip = array(
			'class', 'declare', 'die', 'echo', 'exit', 'for',
			'foreach', 'function', 'global', 'if', 'include',
			'include_once', 'print', 'require', 'require_once',
			'return', 'static', 'switch', 'unset', 'while'
		);

		$okeq = array('===', '!==', '==', '!=', '<=', '>=');

		$code = '';
		$sq = false;
		$dq = false;

		for ($i = 0; $i < strlen($line); $i++)
		{
			$c = $line{$i};
			if ($c == "'")
			{
				$sq = !$sq;
			}
			elseif ($c == '"')
			{
				$dq = !$dq;
			}

			elseif ( ($sq) || ($dq) && $c == "\\")
			{
				++$i;
			}
			else
			{
				$code .= $c;
			}
		}

		$code = str_replace($okeq, '', $code);
		if (strcspn($code, ';{=') != strlen($code))
		{
			return false;
		}

		$kw = preg_split("[^a-z0-9_]i", $code);
		foreach ($kw as $i)
		{
			if (in_array($i, $skip))
			{
				return false;
			}
		}

		return true;
	}

	public static function tab_complete($line, $pos, $cursor)
	{
		$const = array_keys(get_defined_constants());
		$var = array_keys($GLOBALS);
		$func = get_defined_functions();

		foreach ($func["user"] as $i)
		{
				$func["internal"][] = $i;
		}
		$func = $func["internal"];

		return array_merge($const, $var, $func);
	}

	private static function build_date()
	{
		ob_start();
		phpinfo(INFO_GENERAL);

		$x = ob_get_contents();
		ob_end_clean();

		$x = strip_tags($x);
		$x = explode("\n", $x);	// PHP_EOL doesn't work on Windows
		$s = array('Build Date => ', 'Build Date ');

		foreach ($x as $i)
		{
			foreach ($s as $j)
			{
				if (substr($i, 0, strlen($j)) == $j)
				{
					return trim(substr($i, strlen($j)));
				}
			}
		}

		return '???';
	}

}

/* End of file oil/classes/console.php */
