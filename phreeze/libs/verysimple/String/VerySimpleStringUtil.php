<?php
/** @package	verysimple::String */

/**
 * A set of utility functions for working with strings
 *
 * @package	verysimple::String
 * @author Jason Hinkle
 * @copyright  1997-2008 VerySimple, Inc.
 * @license	http://www.gnu.org/licenses/lgpl.html  LGPL
 * @version 1.0
 */
class VerySimpleStringUtil
{
	/** @var the character set used when converting non ascii characters */
	static $DEFAULT_CHARACTER_SET = 'UTF-8';

	/** @var list of fancy/smart quote characters plus emdash w/ generic replacements */
	static $SMART_QUOTE_CHARS;

	/** @var list of xml reserved characters */
	static $XML_SPECIAL_CHARS;

	/** @var associative array containing the html translation for special characters with their numeric equivilant */
	static $HTML_ENTITIES_TABLE;

	/** @var common characters, especially on windows systems, that are technical not valid */
	static $INVALID_CODE_CHARS;

	/** @var characters used as control characters such as escape, backspace, etc */
	static $CONTROL_CODE_CHARS;

	/**
	 * replace the first occurrance only within a string
	 * @param string needle
	 * @param string replacement
	 * @param string haystack
	 */
	static function ReplaceFirst($s,$r,$str)
	{
		$l = strlen($str);
		$a = strpos($str,$s);
		$b = $a + strlen($s);
		$temp = substr($str,0,$a) . $r . substr($str,$b,($l-$b));
		return $temp;
	}

	/**
	 * VerySimpleStringUtil::InitStaticVars(); is called at the bottom of this file
	 */
	static function InitStaticVars()
	{

		self::$HTML_ENTITIES_TABLE = array();
		foreach (get_html_translation_table(HTML_ENTITIES, ENT_QUOTES) as $char => $entity)
		{
			self::$HTML_ENTITIES_TABLE[$entity] = '&#' . ord($char) . ';';
		}

		self::$SMART_QUOTE_CHARS =
			array(
				"�" => "'",
				"�" => "'",
				"�" => "\"",
				"�" => "\"",
				chr(145) => "'",
				chr(146) => "'",
				chr(147) => "\"",
				chr(148) => "\"",
				chr(151) => "-"
			);

		self::$CONTROL_CODE_CHARS =
			array(
				chr(0) => "&#0;",
				chr(1) => "&#1;",
				chr(2) => "&#2;",
				chr(3) => "&#3;",
				chr(4) => "&#4;",
				chr(5) => "&#5;",
				chr(6) => "&#6;",
				chr(7) => "&#7;",
				chr(8) => "&#8;",
				chr(14) => "&#14;",
				chr(15) => "&#15;",
				chr(16) => "&#16;",
				chr(17) => "&#17;",
				chr(18) => "&#18;",
				chr(19) => "&#19;",
				chr(20) => "&#20;",
				chr(21) => "&#21;",
				chr(22) => "&#22;",
				chr(23) => "&#23;",
				chr(24) => "&#24;",
				chr(25) => "&#25;",
				chr(26) => "&#26;",
				chr(27) => "&#27;",
				chr(28) => "&#28;",
				chr(29) => "&#29;",
				chr(30) => "&#30;",
				chr(31) => "&#31;"
			);

		self::$INVALID_CODE_CHARS = array(
			chr(128) => '&#8364;',
			chr(130) => '&#8218;',
			chr(131) => '&#402;',
			chr(132) => '&#8222;',
			chr(133) => '&#8230;',
			chr(134) => '&#8224;',
			chr(135) => '&#8225;',
			chr(136) => '&#710;',
			chr(137) => '&#8240;',
			chr(138) => '&#352;',
			chr(139) => '&#8249;',
			chr(140) => '&#338;',
			chr(142) => '&#381;',
			chr(145) => '&#8216;',
			chr(146) => '&#8217;',
			chr(147) => '&#8220;',
			chr(148) => '&#8221;',
			chr(149) => '&#8226;',
			chr(150) => '&#8211;',
			chr(151) => '&#8212;',
			chr(152) => '&#732;',
			chr(153) => '&#8482;',
			chr(154) => '&#353;',
			chr(155) => '&#8250;',
			chr(156) => '&#339;',
			chr(158) => '&#382;',
			chr(159) => '&#376;');

		self::$XML_SPECIAL_CHARS = array(
			"&"   =>"&amp;",
			"<"   =>"&lt;"
			,">"  =>"&gt;"
			,"\"" =>"&quot;"
			,"'"  =>"&apos;"
		);

	}

	/**
	 * Takes the given text and converts any email address into mailto links,
	 * returning HTML content.
	 *
	 * @param string $text
	 * @param bool true to sanitize the text before parsing for display security
	 * @return string HTML
	 */
	static function ConvertEmailToMailTo($text,$sanitize = false)
	{
		if ($sanitize) $text = VerySimpleStringUtil::Sanitize($text);
		$regex = "/([a-z0-9_\-\.]+)". "@" . "([a-z0-9-]{1,64})" . "\." . "([a-z]{2,10})/i";
		return preg_replace($regex, '<a href="mailto:\\1@\\2.\\3">\\1@\\2.\\3</a>', $text);
	}

	/**
	 * Takes the given text and converts any URLs into links,
	 * returning HTML content.
	 *
	 * @param string $text
	 * @param bool true to sanitize the text before parsing for display security
	 * @return string HTML
	 */
	static function ConvertUrlToLink($text,$sanitize = false)
	{
		if ($sanitize) $text = VerySimpleStringUtil::Sanitize($text);
		$regex = "/[[:alpha:]]+://[^<>[:space:]]+[[:alnum:]/]/i";
		return preg_replace($regex, '<a href=\"\\0\">\\0</a>', $text);
	}

	/**
	 * Sanitize any text so that it can be safely displayed as HTML without
	 * allowing XSS or other injection attacks
	 * @param string $text
	 * @return string
	 */
	static function Sanitize($text)
	{
		return htmlspecialchars($text);
	}

	/**
	 * @param string $string
	 * @param bool $numericEncodingOnly set to true to only use numeric html encoding.  warning, setting to false may be slower performance (default true)
	 * @param bool $encodeControlCharacters (only relevant if $numericEncodingOnly = false) false = wipe control chars.  true = encode control characters (default false)
	 * @return string
	 */
	static function EncodeToHTML($string, $numericEncodingOnly = true, $encodeControlCharacters = false)
	{
		if (strlen($string) == 0) return "";

		$result = $numericEncodingOnly
			? self::UTF8ToHtml($string)
			: self::UTFToNamedHTML($string, $encodeControlCharacters);

		return $result;
	}

	/**
	 * Decode string that has been encoded using EncodeToHTML
	 * used in combination with utf8_decode can be helpful
	 * @TODO: warning, this function is BETA!
	 *
	 * @param string $string
	 * @param destination character set (default = $DEFAULT_CHARACTER_SET (UTF-8))
	 */
	static function DecodeFromHTML($string, $charset = null)
	{
		// this only gets named characters
		// return html_entity_decode($string);

		// this is a complex method that appears to be the reverse of UTF8ToHTML
		// taken from http://www.php.net/manual/en/function.html-entity-decode.php#68491
//		$string = self::ReplaceNonNumericEntities($string);
//		$string = preg_replace_callback('~&(#(x?))?([^;]+);~', 'self::html_entity_replace', $string);
//        return $string;

		// this way at least somebody could specify a character set.  UTF-8 will work most of the time
		if ($charset == null) $charset = VerySimpleStringUtil::$DEFAULT_CHARACTER_SET;
		return mb_convert_encoding($string, $charset, 'HTML-ENTITIES');
	}

	/**
	 * This HTML encodes special characters and returns an ascii safe version.
	 * This function extends EncodeToHTML to additionally strip
	 * out characters that may be disruptive when used in HTML or XML data
	 *
	 * @param string value to parse
	 * @param bool $escapeQuotes true to additionally escape ENT_QUOTE characters <>&"' (default = true)
	 * @param bool $numericEncodingOnly set to true to only use numeric html encoding.  warning, setting to false may be slower performance (default true)
	 * @param bool $replaceSmartQuotes true to replace "smart quotes" with standard ascii ones, can be useful for stripping out windows-only codes (default = false)
	 * @return string
	 */
	static function EncodeSpecialCharacters($string, $escapeQuotes = true, $numericEncodingOnly = true, $replaceSmartQuotes = false)
	{
		if (strlen($string) == 0) return "";

		$result = $string;

		// do this first before encoding
		if ($replaceSmartQuotes) $result = self::ReplaceSmartQuotes($result);

		// this method does not double-encode, but replaces single-quote with a numeric entity
		if ($escapeQuotes) $result = htmlspecialchars($result, ENT_QUOTES, null, false);

		// this method double-encodes values but uses the special character entity for single quotes
		// if ($escapeQuotes) $result = self::ReplaceXMLSpecialChars($result);

		// for special chars we don't need to insist on numeric encoding only
		return self::EncodeToHTML($result,$numericEncodingOnly);

	}

	/**
	 * Converts a string into a character array
	 * @param string $string
	 * @return array
	 */
	static function GetCharArray($string)
	{
		return preg_split("//", $string, -1, PREG_SPLIT_NO_EMPTY);
	}

	/**
	 * This replaces XML special characters with HTML encoding
	 * @param string $string
	 * @return string
	 */
	static function ReplaceXMLSpecialChars($string)
	{
		return strtr($string,self::$XML_SPECIAL_CHARS);
	}

	/**
	 * This replaces smart (fancy) quote characters with generic ascii versions
	 * @param string $string
	 * @return string
	 */
	static function ReplaceSmartQuotes($string)
	{
		return strtr($string,self::$SMART_QUOTE_CHARS);
	}

	/**
	 * This replaces control characters characters with generic ascii versions
	 * @param string $string
	 * @return string
	 */
	static function ReplaceControlCodeChars($string)
	{
		return strtr($string,self::$CONTROL_CODE_CHARS);
	}

	/**
	 * This replaces all non-numeric html entities with the numeric equivilant
	 * @param string $string
	 * @return string
	 */
	static function ReplaceNonNumericEntities($string)
	{
		return strtr($string,self::$HTML_ENTITIES_TABLE);
	}

	/**
	 * This replaces illegal ascii code values $INVALID_CODE_CHARS
	 * @param string $string
	 * @return string
	 */
	static function ReplaceInvalidCodeChars($string)
	{
		return strtr($string,self::$INVALID_CODE_CHARS);
	}

	/**
	 * This is The same as UTFToHTML except it utilizes htmlentities, which will return the Named
	 * HTML code when possible (ie &pound; &sect;, etc).  It is preferrable in all cases to use
	 * UTFToHTML instead unless you absolutely have to have named entities
	 *
	 * @param string $string
	 * @param bool $encodeControlCharacters false = wipe control chars.  true = encode control characters (default false)
	 * @return string
	 */
	static function UTFToNamedHTML($string, $encodeControlCharacters = false)
	{

		$utf8 = $string;
		$result = '';
		for ($i = 0; $i < strlen($utf8); $i++) {
			$char = $utf8[$i];
			$ascii = ord($char);
			if ($ascii < 128) {
				// one-byte character
				$result .= $char;
			} else if ($ascii < 192) {
				// non-utf8 character or not a start byte
				$result .= ($encodeControlCharacters) ? htmlentities($char) : '';
			} else if ($ascii < 224) {
				// two-byte character
				$encoded = htmlentities(substr($utf8, $i, 2), ENT_QUOTES, 'UTF-8');

				// @hack if htmlentities didn't encode it, then we need to do a charset conversion
			   if ($encoded != '' && substr($encoded,0,1) != '&') $encoded = mb_convert_encoding($encoded, 'HTML-ENTITIES', self::$DEFAULT_CHARACTER_SET);

				$result .= $encoded;
				$i++;
			} else if ($ascii < 240) {
				// three-byte character
				$ascii1 = ord($utf8[$i+1]);
				$ascii2 = ord($utf8[$i+2]);
				$unicode = (15 & $ascii) * 4096 +
						   (63 & $ascii1) * 64 +
						   (63 & $ascii2);
				$result .= "&#$unicode;" ;
				$i += 2;
			} else if ($ascii < 248) { // (TODO: should this be 245 or 248 ??)
				// four-byte character
				$ascii1 = ord($utf8[$i+1]);
				$ascii2 = ord($utf8[$i+2]);
				$ascii3 = ord($utf8[$i+3]);
				$unicode = (15 & $ascii) * 262144 +
						   (63 & $ascii1) * 4096 +
						   (63 & $ascii2) * 64 +
						   (63 & $ascii3);
				$result .= "&#$unicode;";
				$i += 3;
			}
		}

		return $result;
	}


	/**
	 * Converts UTF-8 character set into html encoded goodness
	 *
	 * @author montana
	 * @link http://www.php.net/manual/en/function.htmlentities.php#92105
	 * @param string $content
	 */
	static function UTF8ToHTML($content="")
	{
		$contents = self::unicode_string_to_array($content);
		$swap = "";
		$iCount = count($contents);
		for ($o=0;$o<$iCount;$o++) {
			$contents[$o] = self::unicode_entity_replace($contents[$o]);
			$swap .= $contents[$o];
		}
		return mb_convert_encoding($swap,"UTF-8"); //not really necessary, but why not.
	}

	/**
	 * takes a unicode string and turns it into an array
	 * of UTF-8 characters
	 *
	 * @author adjwilli
	 * @param string $string
	 * @return array
	 */
	static function unicode_string_to_array( $string )
	{
		$array = array();
		$strlen = mb_strlen($string);
		while ($strlen) {
			$array[] = mb_substr( $string, 0, 1, "UTF-8" );
			$string = mb_substr( $string, 1, $strlen, "UTF-8" );
			$strlen = mb_strlen( $string );
		}
		return $array;
	}

	/**
	 * Uses scary binary math to replace a character with
	 * it's html entity
	 *
	 * @author m. perez
	 * @param char $c
	 * @return string
	 */
	static function unicode_entity_replace($c)
	{
		$h = ord($c{0});
		if ($h <= 0x7F) { // 127
			return $c;
		} else if ($h < 0xC2) { // 194
			return $c;
		}

		if ($h <= 0xDF) { // 0xDF = 223
			$h = ($h & 0x1F) << 6 | (ord($c{1}) & 0x3F);  // 0x0F = 15, 0x1F = 31, 0x3F = 63
			$h = "&#" . $h . ";";
			return $h;
		} else if ($h <= 0xEF) { // 0xEF = 239
			$h = ($h & 0x0F) << 12 | (ord($c{1}) & 0x3F) << 6 | (ord($c{2}) & 0x3F);
			$h = "&#" . $h . ";";
			return $h;
		} else if ($h <= 0xF4) { // 0xF4 = 244 (TODO: should this be 244 or 247 ??)
			$h = ($h & 0x0F) << 18 | (ord($c{1}) & 0x3F) << 12 | (ord($c{2}) & 0x3F) << 6 | (ord($c{3}) & 0x3F);
			$h = "&#" . $h . ";";
			return $h;
		}
	}

	/**
	 * Used for decoding entities that started as UTF-8
	 * converts a character that is likely non ascii into the correct UTF-8 char value
	 * @link http://www.php.net/manual/en/function.html-entity-decode.php#68491
	 * @param $code
	 */
	function chr_utf8($code)
    {
        if ($code < 0) return false;
        elseif ($code < 128) return chr($code);
        elseif ($code < 160) // Remove Windows Illegals Cars
        {
            if ($code==128) $code=8364;
            elseif ($code==129) $code=160; // not affected
            elseif ($code==130) $code=8218;
            elseif ($code==131) $code=402;
            elseif ($code==132) $code=8222;
            elseif ($code==133) $code=8230;
            elseif ($code==134) $code=8224;
            elseif ($code==135) $code=8225;
            elseif ($code==136) $code=710;
            elseif ($code==137) $code=8240;
            elseif ($code==138) $code=352;
            elseif ($code==139) $code=8249;
            elseif ($code==140) $code=338;
            elseif ($code==141) $code=160; // not affected
            elseif ($code==142) $code=381;
            elseif ($code==143) $code=160; // not affected
            elseif ($code==144) $code=160; // not affected
            elseif ($code==145) $code=8216;
            elseif ($code==146) $code=8217;
            elseif ($code==147) $code=8220;
            elseif ($code==148) $code=8221;
            elseif ($code==149) $code=8226;
            elseif ($code==150) $code=8211;
            elseif ($code==151) $code=8212;
            elseif ($code==152) $code=732;
            elseif ($code==153) $code=8482;
            elseif ($code==154) $code=353;
            elseif ($code==155) $code=8250;
            elseif ($code==156) $code=339;
            elseif ($code==157) $code=160; // not affected
            elseif ($code==158) $code=382;
            elseif ($code==159) $code=376;
        }
        if ($code < 2048) return chr(192 | ($code >> 6)) . chr(128 | ($code & 63));
        elseif ($code < 65536) return chr(224 | ($code >> 12)) . chr(128 | (($code >> 6) & 63)) . chr(128 | ($code & 63));
        else return chr(240 | ($code >> 18)) . chr(128 | (($code >> 12) & 63)) . chr(128 | (($code >> 6) & 63)) . chr(128 | ($code & 63));
    }

    /**
     * Callback for preg_replace_callback('~&(#(x?))?([^;]+);~', 'html_entity_replace', $str);
     * used internally by decode
     * @link http://www.php.net/manual/en/function.html-entity-decode.php#68491
     * @param array
     */
    function html_entity_replace($matches)
    {
        if ($matches[2])
        {
            return self::chr_utf8(hexdec($matches[3]));
        }
        elseif ($matches[1])
        {
            return self::chr_utf8($matches[3]);
        }
        elseif ($matches[3])
        {
        	// return "((&" . $matches[3] . ";))";
        	// return mb_convert_encoding('&'.$matches[3].';', 'UTF-8', 'HTML-ENTITIES');
        	return html_entity_decode('&'.$matches[3].';');
        }

        return false;
    }
}

// this will be executed only once
VerySimpleStringUtil::InitStaticVars();


?>