<?php
/** @package	verysimple::String */

/**
 * A set of utility functions for working with simple template files
 *
 * @package	verysimple::String
 * @author Jason Hinkle
 * @copyright  1997-2011 VerySimple, Inc.
 * @license	http://www.gnu.org/licenses/lgpl.html  LGPL
 * @version 1.0
 */
class SimpleTemplate
{
	
	/** @var used internally for merging. */
	static $_MERGE_TEMPLATE_VALUES = null;
	
	/**
	 * Transforms HTML into formatted plain text.
	 * 
	 * @param string HTML
	 * @return string plain text
	 */
	static function HtmlToText($html)
	{
		require_once("class.html2text.php");
		$h2t = new html2text($html);
		return $h2t->get_text(); 
	}
	
	/**
	 * Transforms plain text into formatted HTML.
	 * 
	 * @param string plain text
	 * @return string HTML
	 */
	static function TextToHtml($txt) 
	{
		//Kills double spaces and spaces inside tags.
		while( !( strpos($txt,'  ') === FALSE ) ) $txt = str_replace('  ',' ',$txt);
		$txt = str_replace(' >','>',$txt);
		$txt = str_replace('< ','<',$txt);
	
		//Transforms accents in html entities.
		$txt = htmlentities($txt);
	
		//We need some HTML entities back!
		$txt = str_replace('&quot;','"',$txt);
		$txt = str_replace('&lt;','<',$txt);
		$txt = str_replace('&gt;','>',$txt);
		$txt = str_replace('&amp;','&',$txt);
	
		//Ajdusts links - anything starting with HTTP opens in a new window
		//$txt = str_ireplace("<a href=\"http://","<a target=\"_blank\" href=\"http://",$txt);
		//$txt = str_ireplace("<a href=http://","<a target=\"_blank\" href=http://",$txt);

		
		//Basic formatting
		$eol = ( strpos($txt,"\r") === FALSE ) ? "\n" : "\r\n";
		$html = '<p>'.str_replace("$eol$eol","</p><p>",$txt).'</p>';
		$html = str_replace("$eol","<br />\n",$html);
		$html = str_replace("</p>","</p>\n\n",$html);
		$html = str_replace("<p></p>","<p>&nbsp;</p>",$html);
	
		//Wipes <br> after block tags (for when the user includes some html in the text).
		$wipebr = Array("table","tr","td","blockquote","ul","ol","li");
	
		for($x = 0; $x < count($wipebr); $x++) 
		{
			$tag = $wipebr[$x];
			$html = str_ireplace("<$tag><br />","<$tag>",$html);
			$html = str_ireplace("</$tag><br />","</$tag>",$html);
		}
	
		return $html;
	}
	
	/**
	 * Merges data into a template with placeholder variables 
	 * (for example "Hello {{NAME}}").  Useful for simple templating
	 * needs such as email, form letters, etc.
	 * 
	 * If a placeholder is in the template but there is no matching value,
	 * then the placeholder will be left alone and will appear in the output.
	 * 
	 * Note that there is no escape character so ensure the right and
	 * left delimiters do not appear as normal text within the template
	 * 
	 * @param string $template string with placeholder variables
	 * @param mixed (array or object) $values an associative array or object with key/value pairs
	 * @param bool true to strip out placeholders with missing value, false to leave them as-is in the output (default true)
	 * @param string the left (opening) delimiter for placeholders. default = {{
	 * @param string the right (closing) delimiter for placeholders. default = }}
	 * @return string merged template
	 */
	static function Merge($template, $values, $stripMissingValues = true, $ldelim = "{{", $rdelim = "}}")
	{
		return $stripMissingValues 
			? self::MergeRegEx($template, $values, $ldelim, $rdelim)
			: self::MergeSimple($template, $values, $ldelim, $rdelim);
	}

	/**
	 * Used internally by Merge, or may be called directly.
	 * If a placeholder is in the template but there is no matching value,
	 * it will be left alone and appear in the template, for example: {{PLACEHOLDER}}.
	 * 
	 * @param string $template string with placeholder variables
	 * @param mixed (array or object) $values an associative array or object with key/value pairs
	 * @param string the left (opening) delimiter for placeholders. default = {{
	 * @param string the right (closing) delimiter for placeholders. default = }}
	 * @return string merged template
	 */
	static function MergeSimple($template, $values, $ldelim = "{{", $rdelim = "}}")
	{
		$replacements = array();
		
		foreach ($values as $key => $val)
		{
			$replacements[$ldelim.$key.$rdelim] = $val;
		}

		return strtr($template, $replacements);
	}
	
	/**
	 * Used internally by Merge, or may be called directly.
	 * If a placeholder is in the template but there is no matching value,
	 * it will be replaced with empty string and will NOT appear in the output.
	 * 
	 * @param string $template string with placeholder variables
	 * @param mixed (array or object) $values an associative array or object with key/value pairs
	 * @param string the left (opening) delimiter for placeholders. default = {{
	 * @param string the right (closing) delimiter for placeholders. default = }}
	 * @return string merged template
	 */
	static function MergeRegEx($template, $values, $ldelim = "{{", $rdelim = "}}")
	{
		self::$_MERGE_TEMPLATE_VALUES = $values;
		
		if ($ldelim != "{{" || $rdelim != "}}") throw new Exception("Custom delimiters are not yet implemented. Sorry!");
		
		$results = preg_replace_callback('!\{\{(\w+)\}\}!', 'SimpleTemplate::_MergeRegExCallback', $template);
		
		self::$_MERGE_TEMPLATE_VALUES = null;
		
		return $results;
		
	}
	
	/**
	 * called internally by preg_replace_callback
	 * @param array $matches
	 */
	static function _MergeRegExCallback($matches)
	{
		if (isset(self::$_MERGE_TEMPLATE_VALUES[$matches[1]]))
		{
			return self::$_MERGE_TEMPLATE_VALUES[$matches[1]];
		}
		else
		{
			return "";
		}
	}
	
}

?>