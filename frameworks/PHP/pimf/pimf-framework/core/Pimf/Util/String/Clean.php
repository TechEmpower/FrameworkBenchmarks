<?php
/**
 * Util
 *
 * @copyright Copyright (c)  Gjero Krsteski (http://krsteski.de)
 * @license   http://krsteski.de/new-bsd-license New BSD License
 */
namespace Pimf\Util\String;

/**
 * String
 *
 * @package Util_String
 * @author  Gjero Krsteski <gjero@krsteski.de>
 */
class Clean
{
  /**
   * An aggressive cleaning - all tags and stuff inside will be removed.
   *
   * @param string $string The string.
   *
   * @return string
   */
  public static function aggressive($string)
  {
    return (string)preg_replace("/<.*?>/", "", (string)$string);
  }

  /**
   * Cleans against XSS.
   *
   * @param string $string  String to check
   * @param string $charset Character set (default ISO-8859-1)
   *
   * @return string $value Sanitized string
   */
  public static function xss($string, $charset = 'ISO-8859-1')
  {
    // Remove Null Characters
    $string = preg_replace(array('/\0+/', '/(\\\\0)+/'), '', $string);

    // Validate standard character entities
    $string = preg_replace('#(&\#*\w+)[\x00-\x20]+;#u', "\\1;", $string);

    // Validate UTF16 two byte encoding (x00)
    $string = preg_replace('#(&\#x*)([0-9A-F]+);*#iu', "\\1\\2;", $string);

    // Just in case stuff like this is submitted: <a href="http://%77%77%77%2E%67%6F%6F%67%6C%65%2E%63%6F%6D">Google</a>
    $string = preg_replace(array("/%u0([a-z0-9]{3})/i", "/%([a-z0-9]{2})/i"), "&#x\\1;", $string);

    // Convert character entities to ASCII
    if (preg_match_all("/<(.+?)>/si", $string, $matches)) {
      for ($i = 0; $i < count($matches['0']); $i++) {
        $string = str_replace(
          $matches['1'][$i], html_entity_decode($matches['1'][$i], ENT_COMPAT, $charset), $string
        );
      }
    }

    // Convert all tabs to spaces
    $string = preg_replace("#\t+#", " ", $string);

    // Makes PHP tags safe
    $string = str_replace(array('<?php', '<?PHP', '<?', '?>'), array('&lt;?php', '&lt;?PHP', '&lt;?', '?&gt;'), $string);

    // Compact any exploded words
    $words = array('javascript', 'vbscript', 'script', 'applet', 'alert', 'document', 'write', 'cookie', 'window');

    foreach ($words as $word) {
      $temp = '';
      for ($i = 0; $i < strlen($word); $i++) {
        $temp .= substr($word, $i, 1) . "\s*";
      }

      $temp   = substr($temp, 0, -3);
      $string = preg_replace('#' . $temp . '#s', $word, $string);
      $string = preg_replace('#' . ucfirst($temp) . '#s', ucfirst($word), $string);
    }

    // Remove disallowed Javascript in links or img tags
    $string = preg_replace(
      "#<a.+?href=.*?(alert\(|alert&\#40;|javascript\:|window\.|document\.|\.cookie|<script|<xss).*?\>.*?</a>#si", "", $string
    );
    $string = preg_replace(
      "#<img.+?src=.*?(alert\(|alert&\#40;|javascript\:|window\.|document\.|\.cookie|<script|<xss).*?\>#si", "", $string
    );
    $string = preg_replace("#<(script|xss).*?\>#si", "", $string);

    // Remove JavaScript Event Handlers
    $string = preg_replace(
      '#(<[^>]+.*?)(onblur|onchange|onclick|onfocus|onload|onmouseover|onmouseup|'
      . 'onmousedown|onselect|onsubmit|onunload|onkeypress|onkeydown|onkeyup|onresize)[^>]*>#iU', "\\1>", $string
    );

    // Sanitize naughty HTML elements
    $string = preg_replace(
      '#<(/*\s*)(alert|applet|basefont|base|behavior|bgsound|'
      . 'blink|body|embed|expression|form|frameset|frame|head|html|ilayer|iframe|input'
      . '|layer|link|meta|object|plaintext|style|script|textarea|title|xml|xss)([^>]*)>#is', "&lt;\\1\\2\\3&gt;", $string
    );

    // Sanitize naughty scripting elements
    $string = preg_replace(
      '#(alert|cmd|passthru|eval|exec|system|fopen|fsockopen|' . 'file|file_get_contents|readfile|unlink)(\s*)\((.*?)\)#si',
      "\\1\\2&#40;\\3&#41;", $string
    );

    // Final clean up
    $bad = array('document.cookie' => '', 'document.write' => '', 'window.location' => '', "javascript\s*:" => '', "Redirect\s+302" => '',
                 '<!--'            => '&lt;!--', '-->' => '--&gt;');

    foreach ($bad as $key => $val) {
      $string = preg_replace("#" . $key . "#i", $val, $string);
    }

    return $string;
  }
}
