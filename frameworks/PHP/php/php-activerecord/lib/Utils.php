<?php
/**
 *
 * @package ActiveRecord
 */

/*
 * Thanks to http://www.eval.ca/articles/php-pluralize (MIT license)
 *           http://dev.rubyonrails.org/browser/trunk/activesupport/lib/active_support/inflections.rb (MIT license)
 *           http://www.fortunecity.com/bally/durrus/153/gramch13.html
 *           http://www2.gsu.edu/~wwwesl/egw/crump.htm
 *
 * Changes (12/17/07)
 *   Major changes
 *   --
 *   Fixed irregular noun algorithm to use regular expressions just like the original Ruby source.
 *       (this allows for things like fireman -> firemen
 *   Fixed the order of the singular array, which was backwards.
 *
 *   Minor changes
 *   --
 *   Removed incorrect pluralization rule for /([^aeiouy]|qu)ies$/ => $1y
 *   Expanded on the list of exceptions for *o -> *oes, and removed rule for buffalo -> buffaloes
 *   Removed dangerous singularization rule for /([^f])ves$/ => $1fe
 *   Added more specific rules for singularizing lives, wives, knives, sheaves, loaves, and leaves and thieves
 *   Added exception to /(us)es$/ => $1 rule for houses => house and blouses => blouse
 *   Added excpetions for feet, geese and teeth
 *   Added rule for deer -> deer
 *
 * Changes:
 *   Removed rule for virus -> viri
 *   Added rule for potato -> potatoes
 *   Added rule for *us -> *uses
 */
namespace ActiveRecord;

use \Closure;

function classify($class_name, $singularize=false)
{
	if ($singularize)
    $class_name = Utils::singularize($class_name);

	$class_name = Inflector::instance()->camelize($class_name);
	return ucfirst($class_name);
}

// http://snippets.dzone.com/posts/show/4660
function array_flatten(array $array)
{
	$i = 0;

	while ($i < count($array))
	{
		if (is_array($array[$i]))
			array_splice($array,$i,1,$array[$i]);
		else
			++$i;
	}
	return $array;
}

/**
 * Somewhat naive way to determine if an array is a hash.
 */
function is_hash(&$array)
{
	if (!is_array($array))
		return false;

	$keys = array_keys($array);
	return @is_string($keys[0]) ? true : false;
}

/**
 * Strips a class name of any namespaces and namespace operator.
 *
 * @param string $class
 * @return string stripped class name
 * @access public
 */
function denamespace($class_name)
{
	if (is_object($class_name))
		$class_name = get_class($class_name);

	if (has_namespace($class_name))
	{
		$parts = explode('\\', $class_name);
		return end($parts);
	}
	return $class_name;
}

function get_namespaces($class_name)
{
	if (has_namespace($class_name))
		return explode('\\', $class_name);
	return null;
}

function has_namespace($class_name)
{
	if (strpos($class_name, '\\') !== false)
		return true;
	return false;
}

/**
 * Returns true if all values in $haystack === $needle
 * @param $needle
 * @param $haystack
 * @return unknown_type
 */
function all($needle, array $haystack)
{
	foreach ($haystack as $value)
	{
		if ($value !== $needle)
			return false;
	}
	return true;
}

function collect(&$enumerable, $name_or_closure)
{
	$ret = array();

	foreach ($enumerable as $value)
	{
		if (is_string($name_or_closure))
			$ret[] = is_array($value) ? $value[$name_or_closure] : $value->$name_or_closure;
		elseif ($name_or_closure instanceof Closure)
			$ret[] = $name_or_closure($value);
	}
	return $ret;
}

/**
 * Wrap string definitions (if any) into arrays.
 */
function wrap_strings_in_arrays(&$strings)
{
	if (!is_array($strings))
		$strings = array(array($strings));
	else 
	{
		foreach ($strings as &$str)
		{
			if (!is_array($str))
				$str = array($str);
		}
	}
	return $strings;
}

/**
 * Some internal utility functions.
 *
 * @package ActiveRecord
 */
class Utils
{
	public static function extract_options($options)
	{
		return is_array(end($options)) ? end($options) : array();
	}

	public static function add_condition(&$conditions=array(), $condition, $conjuction='AND')
	{
		if (is_array($condition))
		{
			if (empty($conditions))
				$conditions = array_flatten($condition);
			else
			{
				$conditions[0] .= " $conjuction " . array_shift($condition);
				$conditions[] = array_flatten($condition);
			}
		}
		elseif (is_string($condition))
			$conditions[0] .= " $conjuction $condition";

		return $conditions;
	}

	public static function human_attribute($attr)
	{
		$inflector = Inflector::instance();
		$inflected = $inflector->variablize($attr);
		$normal = $inflector->uncamelize($inflected);

		return ucfirst(str_replace('_', ' ', $normal));
	}

	public static function is_odd($number)
	{
		return $number & 1;
	}

	public static function is_a($type, $var)
	{
		switch($type)
		{
			case 'range':
				if (is_array($var) && (int)$var[0] < (int)$var[1])
					return true;

		}

		return false;
	}

	public static function is_blank($var)
	{
		return 0 === strlen($var);
	}

	private static $plural = array(
        '/(quiz)$/i'               => "$1zes",
        '/^(ox)$/i'                => "$1en",
        '/([m|l])ouse$/i'          => "$1ice",
        '/(matr|vert|ind)ix|ex$/i' => "$1ices",
        '/(x|ch|ss|sh)$/i'         => "$1es",
        '/([^aeiouy]|qu)y$/i'      => "$1ies",
        '/(hive)$/i'               => "$1s",
        '/(?:([^f])fe|([lr])f)$/i' => "$1$2ves",
        '/(shea|lea|loa|thie)f$/i' => "$1ves",
        '/sis$/i'                  => "ses",
        '/([ti])um$/i'             => "$1a",
        '/(tomat|potat|ech|her|vet)o$/i'=> "$1oes",
        '/(bu)s$/i'                => "$1ses",
        '/(alias)$/i'              => "$1es",
        '/(octop)us$/i'            => "$1i",
        '/(ax|test)is$/i'          => "$1es",
        '/(us)$/i'                 => "$1es",
        '/s$/i'                    => "s",
        '/$/'                      => "s"
    );

    private static $singular = array(
        '/(quiz)zes$/i'             => "$1",
        '/(matr)ices$/i'            => "$1ix",
        '/(vert|ind)ices$/i'        => "$1ex",
        '/^(ox)en$/i'               => "$1",
        '/(alias)es$/i'             => "$1",
        '/(octop|vir)i$/i'          => "$1us",
        '/(cris|ax|test)es$/i'      => "$1is",
        '/(shoe)s$/i'               => "$1",
        '/(o)es$/i'                 => "$1",
        '/(bus)es$/i'               => "$1",
        '/([m|l])ice$/i'            => "$1ouse",
        '/(x|ch|ss|sh)es$/i'        => "$1",
        '/(m)ovies$/i'              => "$1ovie",
        '/(s)eries$/i'              => "$1eries",
        '/([^aeiouy]|qu)ies$/i'     => "$1y",
        '/([lr])ves$/i'             => "$1f",
        '/(tive)s$/i'               => "$1",
        '/(hive)s$/i'               => "$1",
        '/(li|wi|kni)ves$/i'        => "$1fe",
        '/(shea|loa|lea|thie)ves$/i'=> "$1f",
        '/(^analy)ses$/i'           => "$1sis",
        '/((a)naly|(b)a|(d)iagno|(p)arenthe|(p)rogno|(s)ynop|(t)he)ses$/i'  => "$1$2sis",
        '/([ti])a$/i'               => "$1um",
        '/(n)ews$/i'                => "$1ews",
        '/(h|bl)ouses$/i'           => "$1ouse",
        '/(corpse)s$/i'             => "$1",
        '/(us)es$/i'                => "$1",
        '/(us|ss)$/i'               => "$1",
        '/s$/i'                     => ""
    );

    private static $irregular = array(
        'move'   => 'moves',
        'foot'   => 'feet',
        'goose'  => 'geese',
        'sex'    => 'sexes',
        'child'  => 'children',
        'man'    => 'men',
        'tooth'  => 'teeth',
        'person' => 'people'
    );

    private static $uncountable = array(
        'sheep',
        'fish',
        'deer',
        'series',
        'species',
        'money',
        'rice',
        'information',
        'equipment'
    );

    public static function pluralize( $string )
    {
        // save some time in the case that singular and plural are the same
        if ( in_array( strtolower( $string ), self::$uncountable ) )
            return $string;

        // check for irregular singular forms
        foreach ( self::$irregular as $pattern => $result )
        {
            $pattern = '/' . $pattern . '$/i';

            if ( preg_match( $pattern, $string ) )
                return preg_replace( $pattern, $result, $string);
        }

        // check for matches using regular expressions
        foreach ( self::$plural as $pattern => $result )
        {
            if ( preg_match( $pattern, $string ) )
                return preg_replace( $pattern, $result, $string );
        }

        return $string;
    }

    public static function singularize( $string )
    {
        // save some time in the case that singular and plural are the same
        if ( in_array( strtolower( $string ), self::$uncountable ) )
            return $string;

        // check for irregular plural forms
        foreach ( self::$irregular as $result => $pattern )
        {
            $pattern = '/' . $pattern . '$/i';

            if ( preg_match( $pattern, $string ) )
                return preg_replace( $pattern, $result, $string);
        }

        // check for matches using regular expressions
        foreach ( self::$singular as $pattern => $result )
        {
            if ( preg_match( $pattern, $string ) )
                return preg_replace( $pattern, $result, $string );
        }

        return $string;
    }

    public static function pluralize_if($count, $string)
    {
        if ($count == 1)
            return $string;
        else
            return self::pluralize($string);
    }

	public static function squeeze($char, $string)
	{
		return preg_replace("/$char+/",$char,$string);
	}
};
?>
