<?php
/**
 * @package ActiveRecord
 */
namespace ActiveRecord;

/**
 * @package ActiveRecord
 */
abstract class Inflector
{
	/**
	 * Get an instance of the {@link Inflector} class.
	 *
	 * @return object
	 */
	public static function instance()
	{
		return new StandardInflector();
	}

	/**
	 * Turn a string into its camelized version.
	 *
	 * @param string $s string to convert
	 * @return string
	 */
	public function camelize($s)
	{
		$s = preg_replace('/[_-]+/','_',trim($s));
		$s = str_replace(' ', '_', $s);

		$camelized = '';

		for ($i=0,$n=strlen($s); $i<$n; ++$i)
		{
			if ($s[$i] == '_' && $i+1 < $n)
				$camelized .= strtoupper($s[++$i]);
			else
				$camelized .= $s[$i];
		}

		$camelized = trim($camelized,' _');

		if (strlen($camelized) > 0)
			$camelized[0] = strtolower($camelized[0]);

		return $camelized;
	}

	/**
	 * Determines if a string contains all uppercase characters.
	 *
	 * @param string $s string to check
	 * @return bool
	 */
	public static function is_upper($s)
	{
		return (strtoupper($s) === $s);
	}

	/**
	 * Determines if a string contains all lowercase characters.
	 *
	 * @param string $s string to check
	 * @return bool
	 */
	public static function is_lower($s)
	{
		return (strtolower($s) === $s);
	}

	/**
	 * Convert a camelized string to a lowercase, underscored string.
	 *
	 * @param string $s string to convert
	 * @return string
	 */
	public function uncamelize($s)
	{
		$normalized = '';

		for ($i=0,$n=strlen($s); $i<$n; ++$i)
		{
			if (ctype_alpha($s[$i]) && self::is_upper($s[$i]))
				$normalized .= '_' . strtolower($s[$i]);
			else
				$normalized .= $s[$i];
		}
		return trim($normalized,' _');
	}

	/**
	 * Convert a string with space into a underscored equivalent.
	 *
	 * @param string $s string to convert
	 * @return string
	 */
	public function underscorify($s)
	{
		return preg_replace(array('/[_\- ]+/','/([a-z])([A-Z])/'),array('_','\\1_\\2'),trim($s));
	}

	public function keyify($class_name)
	{
		return strtolower($this->underscorify(denamespace($class_name))) . '_id';
	}

	abstract function variablize($s);
}

/**
 * @package ActiveRecord
 */
class StandardInflector extends Inflector
{
	public function tableize($s) { return Utils::pluralize(strtolower($this->underscorify($s))); }
	public function variablize($s) { return str_replace(array('-',' '),array('_','_'),strtolower(trim($s))); }
}
?>
