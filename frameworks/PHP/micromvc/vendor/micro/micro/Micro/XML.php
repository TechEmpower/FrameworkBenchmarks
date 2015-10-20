<?php
/**
 * XML
 *
 * Converts any variable type (arrays, objects, strings) to a SimpleXML object.
 *
 * @package		MicroMVC
 * @author		David Pennington
 * @copyright	(c) 2011 MicroMVC Framework
 * @license		http://micromvc.com/license
 ********************************** 80 Columns *********************************
 */
namespace Micro;

class XML
{

	/**
	 * Convert any given variable into a SimpleXML object
	 *
	 * @param mixed $object variable object to convert
	 * @param string $root root element name
	 * @param object $xml xml object
	 * @param string $unknown element name for numeric keys
	 * @param string $doctype XML doctype
	 */
	public static function from($object, $root = 'data', $xml = NULL, $unknown = 'element', $doctype = "<?xml version = '1.0' encoding = 'utf-8'?>")
	{
		if(is_null($xml))
		{
			$xml = simplexml_load_string("$doctype<$root/>");
		}

		foreach((array) $object as $k => $v)
		{
			if(is_int($k))
			{
				$k = $unknown;
			}

			if(is_scalar($v))
			{
				$xml->addChild($k, h($v));
			}
			else
			{
				$v = (array) $v;
				$node = array_diff_key($v, array_keys(array_keys($v))) ? $xml->addChild($k) : $xml;
				self::from($v, $k, $node);
			}
		}

		return $xml;
	}

}

// END
