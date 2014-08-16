<?php defined('SYSPATH') OR die('No direct script access.');
/**
 * The Kohana_HTTP_Header class provides an Object-Orientated interface
 * to HTTP headers. This can parse header arrays returned from the
 * PHP functions `apache_request_headers()` or the `http_parse_headers()`
 * function available within the PECL HTTP library.
 *
 * @package    Kohana
 * @category   HTTP
 * @author     Kohana Team
 * @since      3.1.0
 * @copyright  (c) 2008-2012 Kohana Team
 * @license    http://kohanaphp.com/license
 */
class Kohana_HTTP_Header extends ArrayObject {

	// Default Accept-* quality value if none supplied
	const DEFAULT_QUALITY = 1;

	/**
	 * Parses an Accept(-*) header and detects the quality
	 *
	 * @param   array   $parts  accept header parts
	 * @return  array
	 * @since   3.2.0
	 */
	public static function accept_quality(array $parts)
	{
		$parsed = array();

		// Resource light iteration
		$parts_keys = array_keys($parts);
		foreach ($parts_keys as $key)
		{
			$value = trim(str_replace(array("\r", "\n"), '', $parts[$key]));

			$pattern = '~\b(\;\s*+)?q\s*+=\s*+([.0-9]+)~';

			// If there is no quality directive, return default
			if ( ! preg_match($pattern, $value, $quality))
			{
				$parsed[$value] = (float) HTTP_Header::DEFAULT_QUALITY;
			}
			else
			{
				$quality = $quality[2];

				if ($quality[0] === '.')
				{
					$quality = '0'.$quality;
				}

				// Remove the quality value from the string and apply quality
				$parsed[trim(preg_replace($pattern, '', $value, 1), '; ')] = (float) $quality;
			}
		}

		return $parsed;
	}

	/**
	 * Parses the accept header to provide the correct quality values
	 * for each supplied accept type.
	 *
	 * @link    http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.1
	 * @param   string  $accepts    accept content header string to parse
	 * @return  array
	 * @since   3.2.0
	 */
	public static function parse_accept_header($accepts = NULL)
	{
		$accepts = explode(',', (string) $accepts);

		// If there is no accept, lets accept everything
		if ($accepts === NULL)
			return array('*' => array('*' => (float) HTTP_Header::DEFAULT_QUALITY));

		// Parse the accept header qualities
		$accepts = HTTP_Header::accept_quality($accepts);

		$parsed_accept = array();

		// This method of iteration uses less resource
		$keys = array_keys($accepts);
		foreach ($keys as $key)
		{
			// Extract the parts
			$parts = explode('/', $key, 2);

			// Invalid content type- bail
			if ( ! isset($parts[1]))
				continue;

			// Set the parsed output
			$parsed_accept[$parts[0]][$parts[1]] = $accepts[$key];
		}

		return $parsed_accept;
	}

	/**
	 * Parses the `Accept-Charset:` HTTP header and returns an array containing
	 * the charset and associated quality.
	 *
	 * @link    http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.2
	 * @param   string  $charset    charset string to parse
	 * @return  array
	 * @since   3.2.0
	 */
	public static function parse_charset_header($charset = NULL)
	{
		if ($charset === NULL)
		{
			return array('*' => (float) HTTP_Header::DEFAULT_QUALITY);
		}

		return HTTP_Header::accept_quality(explode(',', (string) $charset));
	}

	/**
	 * Parses the `Accept-Encoding:` HTTP header and returns an array containing
	 * the charsets and associated quality.
	 *
	 * @link    http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.3
	 * @param   string  $encoding   charset string to parse
	 * @return  array
	 * @since   3.2.0
	 */
	public static function parse_encoding_header($encoding = NULL)
	{
		// Accept everything
		if ($encoding === NULL)
		{
			return array('*' => (float) HTTP_Header::DEFAULT_QUALITY);
		}
		elseif ($encoding === '')
		{
			return array('identity' => (float) HTTP_Header::DEFAULT_QUALITY);
		}
		else
		{
			return HTTP_Header::accept_quality(explode(',', (string) $encoding));
		}
	}

	/**
	 * Parses the `Accept-Language:` HTTP header and returns an array containing
	 * the languages and associated quality.
	 *
	 * @link    http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.4
	 * @param   string  $language   charset string to parse
	 * @return  array
	 * @since   3.2.0
	 */
	public static function parse_language_header($language = NULL)
	{
		if ($language === NULL)
		{
			return array('*' => array('*' => (float) HTTP_Header::DEFAULT_QUALITY));
		}

		$language = HTTP_Header::accept_quality(explode(',', (string) $language));

		$parsed_language = array();

		$keys = array_keys($language);
		foreach ($keys as $key)
		{
			// Extract the parts
			$parts = explode('-', $key, 2);

			// Invalid content type- bail
			if ( ! isset($parts[1]))
			{
				$parsed_language[$parts[0]]['*'] = $language[$key];
			}
			else
			{
				// Set the parsed output
				$parsed_language[$parts[0]][$parts[1]] = $language[$key];
			}
		}

		return $parsed_language;
	}

	/**
	 * Generates a Cache-Control HTTP header based on the supplied array.
	 *
	 *     // Set the cache control headers you want to use
	 *     $cache_control = array(
	 *         'max-age'          => 3600,
	 *         'must-revalidate',
	 *         'public'
	 *     );
	 *
	 *     // Create the cache control header, creates :
	 *     // cache-control: max-age=3600, must-revalidate, public
	 *     $response->headers('Cache-Control', HTTP_Header::create_cache_control($cache_control);
	 *
	 * @link    http://www.w3.org/Protocols/rfc2616/rfc2616-sec13.html#sec13
	 * @param   array   $cache_control  Cache-Control to render to string
	 * @return  string
	 */
	public static function create_cache_control(array $cache_control)
	{
		$parts = array();

		foreach ($cache_control as $key => $value)
		{
			$parts[] = (is_int($key)) ? $value : ($key.'='.$value);
		}

		return implode(', ', $parts);
	}

	/**
	 * Parses the Cache-Control header and returning an array representation of the Cache-Control
	 * header.
	 *
	 *     // Create the cache control header
	 *     $response->headers('cache-control', 'max-age=3600, must-revalidate, public');
	 *
	 *     // Parse the cache control header
	 *     if ($cache_control = HTTP_Header::parse_cache_control($response->headers('cache-control')))
	 *     {
	 *          // Cache-Control header was found
	 *          $maxage = $cache_control['max-age'];
	 *     }
	 *
	 * @param   array   $cache_control Array of headers
	 * @return  mixed
	 */
	public static function parse_cache_control($cache_control)
	{
		$directives = explode(',', strtolower($cache_control));

		if ($directives === FALSE)
			return FALSE;

		$output = array();

		foreach ($directives as $directive)
		{
			if (strpos($directive, '=') !== FALSE)
			{
				list($key, $value) = explode('=', trim($directive), 2);

				$output[$key] = ctype_digit($value) ? (int) $value : $value;
			}
			else
			{
				$output[] = trim($directive);
			}
		}

		return $output;
	}

	/**
	 * @var     array    Accept: (content) types
	 */
	protected $_accept_content;

	/**
	 * @var     array    Accept-Charset: parsed header
	 */
	protected $_accept_charset;

	/**
	 * @var     array    Accept-Encoding: parsed header
	 */
	protected $_accept_encoding;

	/**
	 * @var     array    Accept-Language: parsed header
	 */
	protected $_accept_language;

	/**
	 * Constructor method for [Kohana_HTTP_Header]. Uses the standard constructor
	 * of the parent `ArrayObject` class.
	 *
	 *     $header_object = new HTTP_Header(array('x-powered-by' => 'Kohana 3.1.x', 'expires' => '...'));
	 *
	 * @param   mixed   $input          Input array
	 * @param   int     $flags          Flags
	 * @param   string  $iterator_class The iterator class to use
	 */
	public function __construct(array $input = array(), $flags = NULL, $iterator_class = 'ArrayIterator')
	{
		/**
		 * @link http://www.w3.org/Protocols/rfc2616/rfc2616.html
		 *
		 * HTTP header declarations should be treated as case-insensitive
		 */
		$input = array_change_key_case( (array) $input, CASE_LOWER);

		parent::__construct($input, $flags, $iterator_class);
	}

	/**
	 * Returns the header object as a string, including
	 * the terminating new line
	 *
	 *     // Return the header as a string
	 *     echo (string) $request->headers();
	 *
	 * @return  string
	 */
	public function __toString()
	{
		$header = '';

		foreach ($this as $key => $value)
		{
			// Put the keys back the Case-Convention expected
			$key = Text::ucfirst($key);

			if (is_array($value))
			{
				$header .= $key.': '.(implode(', ', $value))."\r\n";
			}
			else
			{
				$header .= $key.': '.$value."\r\n";
			}
		}

		return $header."\r\n";
	}

	/**
	 * Overloads `ArrayObject::offsetSet()` to enable handling of header
	 * with multiple instances of the same directive. If the `$replace` flag
	 * is `FALSE`, the header will be appended rather than replacing the
	 * original setting.
	 *
	 * @param   mixed   $index      index to set `$newval` to
	 * @param   mixed   $newval     new value to set
	 * @param   boolean $replace    replace existing value
	 * @return  void
	 * @since   3.2.0
	 */
	public function offsetSet($index, $newval, $replace = TRUE)
	{
		// Ensure the index is lowercase
		$index = strtolower($index);

		if ($replace OR ! $this->offsetExists($index))
		{
			return parent::offsetSet($index, $newval);
		}

		$current_value = $this->offsetGet($index);

		if (is_array($current_value))
		{
			$current_value[] = $newval;
		}
		else
		{
			$current_value = array($current_value, $newval);
		}

		return parent::offsetSet($index, $current_value);
	}

	/**
	 * Overloads the `ArrayObject::offsetExists()` method to ensure keys
	 * are lowercase.
	 *
	 * @param   string  $index
	 * @return  boolean
	 * @since   3.2.0
	 */
	public function offsetExists($index)
	{
		return parent::offsetExists(strtolower($index));
	}

	/**
	 * Overloads the `ArrayObject::offsetUnset()` method to ensure keys
	 * are lowercase.
	 *
	 * @param   string  $index
	 * @return  void
	 * @since   3.2.0
	 */
	public function offsetUnset($index)
	{
		return parent::offsetUnset(strtolower($index));
	}

	/**
	 * Overload the `ArrayObject::offsetGet()` method to ensure that all
	 * keys passed to it are formatted correctly for this object.
	 *
	 * @param   string  $index  index to retrieve
	 * @return  mixed
	 * @since   3.2.0
	 */
	public function offsetGet($index)
	{
		return parent::offsetGet(strtolower($index));
	}

	/**
	 * Overloads the `ArrayObject::exchangeArray()` method to ensure that
	 * all keys are changed to lowercase.
	 *
	 * @param   mixed   $input
	 * @return  array
	 * @since   3.2.0
	 */
	public function exchangeArray($input)
	{
		/**
		 * @link http://www.w3.org/Protocols/rfc2616/rfc2616.html
		 *
		 * HTTP header declarations should be treated as case-insensitive
		 */
		$input = array_change_key_case( (array) $input, CASE_LOWER);

		return parent::exchangeArray($input);
	}

	/**
	 * Parses a HTTP Message header line and applies it to this HTTP_Header
	 *
	 *     $header = $response->headers();
	 *     $header->parse_header_string(NULL, 'content-type: application/json');
	 *
	 * @param   resource    $resource       the resource (required by Curl API)
	 * @param   string      $header_line    the line from the header to parse
	 * @return  int
	 * @since   3.2.0
	 */
	public function parse_header_string($resource, $header_line)
	{
		$headers = array();

		if (preg_match_all('/(\w[^\s:]*):[ ]*([^\r\n]*(?:\r\n[ \t][^\r\n]*)*)/', $header_line, $matches))
		{
			foreach ($matches[0] as $key => $value)
			{
				$this->offsetSet($matches[1][$key], $matches[2][$key], FALSE);
			}
		}

		return strlen($header_line);
	}

	/**
	 * Returns the accept quality of a submitted mime type based on the
	 * request `Accept:` header. If the `$explicit` argument is `TRUE`,
	 * only precise matches will be returned, excluding all wildcard (`*`)
	 * directives.
	 *
	 *     // Accept: application/xml; application/json; q=.5; text/html; q=.2, text/*
	 *     // Accept quality for application/json
	 *
	 *     // $quality = 0.5
	 *     $quality = $request->headers()->accepts_at_quality('application/json');
	 *
	 *     // $quality_explicit = FALSE
	 *     $quality_explicit = $request->headers()->accepts_at_quality('text/plain', TRUE);
	 *
	 * @param   string  $type
	 * @param   boolean $explicit   explicit check, excludes `*`
	 * @return  mixed
	 * @since   3.2.0
	 */
	public function accepts_at_quality($type, $explicit = FALSE)
	{
		// Parse Accept header if required
		if ($this->_accept_content === NULL)
		{
			if ($this->offsetExists('Accept'))
			{
				$accept = $this->offsetGet('Accept');
			}
			else
			{
				$accept = '*/*';
			}

			$this->_accept_content = HTTP_Header::parse_accept_header($accept);
		}

		// If not a real mime, try and find it in config
		if (strpos($type, '/') === FALSE)
		{
			$mime = Kohana::$config->load('mimes.'.$type);

			if ($mime === NULL)
				return FALSE;

			$quality = FALSE;

			foreach ($mime as $_type)
			{
				$quality_check = $this->accepts_at_quality($_type, $explicit);
				$quality = ($quality_check > $quality) ? $quality_check : $quality;
			}

			return $quality;
		}

		$parts = explode('/', $type, 2);

		if (isset($this->_accept_content[$parts[0]][$parts[1]]))
		{
			return $this->_accept_content[$parts[0]][$parts[1]];
		}
		elseif ($explicit === TRUE)
		{
			return FALSE;
		}
		else
		{
			if (isset($this->_accept_content[$parts[0]]['*']))
			{
				return $this->_accept_content[$parts[0]]['*'];
			}
			elseif (isset($this->_accept_content['*']['*']))
			{
				return $this->_accept_content['*']['*'];
			}
			else
			{
				return FALSE;
			}
		}
	}

	/**
	 * Returns the preferred response content type based on the accept header
	 * quality settings. If items have the same quality value, the first item
	 * found in the array supplied as `$types` will be returned.
	 *
	 *     // Get the preferred acceptable content type
	 *     // Accept: text/html, application/json; q=.8, text/*
	 *     $result = $header->preferred_accept(array(
	 *         'text/html'
	 *         'text/rtf',
	 *         'application/json'
	 *     )); // $result = 'application/json'
	 *
	 *     $result = $header->preferred_accept(array(
	 *         'text/rtf',
	 *         'application/xml'
	 *     ), TRUE); // $result = FALSE (none matched explicitly)
	 *
	 *
	 * @param   array   $types      the content types to examine
	 * @param   boolean $explicit   only allow explicit references, no wildcards
	 * @return  string  name of the preferred content type
	 * @since   3.2.0
	 */
	public function preferred_accept(array $types, $explicit = FALSE)
	{
		$preferred = FALSE;
		$ceiling = 0;

		foreach ($types as $type)
		{
			$quality = $this->accepts_at_quality($type, $explicit);

			if ($quality > $ceiling)
			{
				$preferred = $type;
				$ceiling = $quality;
			}
		}

		return $preferred;
	}

	/**
	 * Returns the quality of the supplied `$charset` argument. This method
	 * will automatically parse the `Accept-Charset` header if present and
	 * return the associated resolved quality value.
	 *
	 *      // Accept-Charset: utf-8, utf-16; q=.8, iso-8859-1; q=.5
	 *      $quality = $header->accepts_charset_at_quality('utf-8');
	 *            // $quality = (float) 1
	 *
	 * @param   string  $charset    charset to examine
	 * @return  float   the quality of the charset
	 * @since   3.2.0
	 */
	public function accepts_charset_at_quality($charset)
	{
		if ($this->_accept_charset === NULL)
		{
			if ($this->offsetExists('Accept-Charset'))
			{
				$charset_header = strtolower($this->offsetGet('Accept-Charset'));
				$this->_accept_charset = HTTP_Header::parse_charset_header($charset_header);
			}
			else
			{
				$this->_accept_charset = HTTP_Header::parse_charset_header(NULL);
			}
		}

		$charset = strtolower($charset);

		if (isset($this->_accept_charset[$charset]))
		{
			return $this->_accept_charset[$charset];
		}
		elseif (isset($this->_accept_charset['*']))
		{
			return $this->_accept_charset['*'];
		}
		elseif ($charset === 'iso-8859-1')
		{
			return (float) 1;
		}

		return (float) 0;
	}

	/**
	 * Returns the preferred charset from the supplied array `$charsets` based
	 * on the `Accept-Charset` header directive.
	 *
	 *      // Accept-Charset: utf-8, utf-16; q=.8, iso-8859-1; q=.5
	 *      $charset = $header->preferred_charset(array(
	 *          'utf-10', 'ascii', 'utf-16', 'utf-8'
	 *      )); // $charset = 'utf-8'
	 *
	 * @param   array   $charsets   charsets to test
	 * @return  mixed   preferred charset or `FALSE`
	 * @since   3.2.0
	 */
	public function preferred_charset(array $charsets)
	{
		$preferred = FALSE;
		$ceiling = 0;

		foreach ($charsets as $charset)
		{
			$quality = $this->accepts_charset_at_quality($charset);

			if ($quality > $ceiling)
			{
				$preferred = $charset;
				$ceiling = $quality;
			}
		}

		return $preferred;
	}

	/**
	 * Returns the quality of the `$encoding` type passed to it. Encoding
	 * is usually compression such as `gzip`, but could be some other
	 * message encoding algorithm. This method allows explicit checks to be
	 * done ignoring wildcards.
	 *
	 *      // Accept-Encoding: compress, gzip, *; q=.5
	 *      $encoding = $header->accepts_encoding_at_quality('gzip');
	 *      // $encoding = (float) 1.0s
	 *
	 * @param   string  $encoding   encoding type to interrogate
	 * @param   boolean $explicit   explicit check, ignoring wildcards and `identity`
	 * @return  float
	 * @since   3.2.0
	 */
	public function accepts_encoding_at_quality($encoding, $explicit = FALSE)
	{
		if ($this->_accept_encoding === NULL)
		{
			if ($this->offsetExists('Accept-Encoding'))
			{
				$encoding_header = $this->offsetGet('Accept-Encoding');
			}
			else
			{
				$encoding_header = NULL;
			}

			$this->_accept_encoding = HTTP_Header::parse_encoding_header($encoding_header);
		}

		// Normalize the encoding
		$encoding = strtolower($encoding);

		if (isset($this->_accept_encoding[$encoding]))
		{
			return $this->_accept_encoding[$encoding];
		}

		if ($explicit === FALSE)
		{
			if (isset($this->_accept_encoding['*']))
			{
				return $this->_accept_encoding['*'];
			}
			elseif ($encoding === 'identity')
			{
				return (float) HTTP_Header::DEFAULT_QUALITY;
			}
		}

		return (float) 0;
	}

	/**
	 * Returns the preferred message encoding type based on quality, and can
	 * optionally ignore wildcard references. If two or more encodings have the
	 * same quality, the first listed in `$encodings` will be returned.
	 *
	 *     // Accept-Encoding: compress, gzip, *; q.5
	 *     $encoding = $header->preferred_encoding(array(
	 *          'gzip', 'bzip', 'blowfish'
	 *     ));
	 *     // $encoding = 'gzip';
	 *
	 * @param   array   $encodings  encodings to test against
	 * @param   boolean $explicit   explicit check, if `TRUE` wildcards are excluded
	 * @return  mixed
	 * @since   3.2.0
	 */
	public function preferred_encoding(array $encodings, $explicit = FALSE)
	{
		$ceiling = 0;
		$preferred = FALSE;

		foreach ($encodings as $encoding)
		{
			$quality = $this->accepts_encoding_at_quality($encoding, $explicit);

			if ($quality > $ceiling)
			{
				$ceiling = $quality;
				$preferred = $encoding;
			}
		}

		return $preferred;
	}

	/**
	 * Returns the quality of `$language` supplied, optionally ignoring
	 * wildcards if `$explicit` is set to a non-`FALSE` value. If the quality
	 * is not found, `0.0` is returned.
	 *
	 *     // Accept-Language: en-us, en-gb; q=.7, en; q=.5
	 *     $lang = $header->accepts_language_at_quality('en-gb');
	 *     // $lang = (float) 0.7
	 *
	 *     $lang2 = $header->accepts_language_at_quality('en-au');
	 *     // $lang2 = (float) 0.5
	 *
	 *     $lang3 = $header->accepts_language_at_quality('en-au', TRUE);
	 *     // $lang3 = (float) 0.0
	 *
	 * @param   string  $language   language to interrogate
	 * @param   boolean $explicit   explicit interrogation, `TRUE` ignores wildcards
	 * @return  float
	 * @since   3.2.0
	 */
	public function accepts_language_at_quality($language, $explicit = FALSE)
	{
		if ($this->_accept_language === NULL)
		{
			if ($this->offsetExists('Accept-Language'))
			{
				$language_header = strtolower($this->offsetGet('Accept-Language'));
			}
			else
			{
				$language_header = NULL;
			}

			$this->_accept_language = HTTP_Header::parse_language_header($language_header);
		}

		// Normalize the language
		$language_parts = explode('-', strtolower($language), 2);

		if (isset($this->_accept_language[$language_parts[0]]))
		{
			if (isset($language_parts[1]))
			{
				if (isset($this->_accept_language[$language_parts[0]][$language_parts[1]]))
				{
					return $this->_accept_language[$language_parts[0]][$language_parts[1]];
				}
				elseif ($explicit === FALSE AND isset($this->_accept_language[$language_parts[0]]['*']))
				{
					return $this->_accept_language[$language_parts[0]]['*'];
				}
			}
			elseif (isset($this->_accept_language[$language_parts[0]]['*']))
			{
				return $this->_accept_language[$language_parts[0]]['*'];
			}
		}

		if ($explicit === FALSE AND isset($this->_accept_language['*']))
		{
			return $this->_accept_language['*'];
		}

		return (float) 0;
	}

	/**
	 * Returns the preferred language from the supplied array `$languages` based
	 * on the `Accept-Language` header directive.
	 *
	 *      // Accept-Language: en-us, en-gb; q=.7, en; q=.5
	 *      $lang = $header->preferred_language(array(
	 *          'en-gb', 'en-au', 'fr', 'es'
	 *      )); // $lang = 'en-gb'
	 *
	 * @param   array   $languages
	 * @param   boolean $explicit
	 * @return  mixed
	 * @since   3.2.0
	 */
	public function preferred_language(array $languages, $explicit = FALSE)
	{
		$ceiling = 0;
		$preferred = FALSE;

		foreach ($languages as $language)
		{
			$quality = $this->accepts_language_at_quality($language, $explicit);

			if ($quality > $ceiling)
			{
				$ceiling = $quality;
				$preferred = $language;
			}
		}

		return $preferred;
	}

	/**
	 * Sends headers to the php processor, or supplied `$callback` argument.
	 * This method formats the headers correctly for output, re-instating their
	 * capitalization for transmission.
	 *
	 * [!!] if you supply a custom header handler via `$callback`, it is
	 *  recommended that `$response` is returned
	 *
	 * @param   HTTP_Response   $response   header to send
	 * @param   boolean         $replace    replace existing value
	 * @param   callback        $callback   optional callback to replace PHP header function
	 * @return  mixed
	 * @since   3.2.0
	 */
	public function send_headers(HTTP_Response $response = NULL, $replace = FALSE, $callback = NULL)
	{
		if ($response === NULL)
		{
			// Default to the initial request message
			$response = Request::initial()->response();
		}

		$protocol = $response->protocol();
		$status = $response->status();

		// Create the response header
		$processed_headers = array($protocol.' '.$status.' '.Response::$messages[$status]);

		// Get the headers array
		$headers = $response->headers()->getArrayCopy();

		foreach ($headers as $header => $value)
		{
			if (is_array($value))
			{
				$value = implode(', ', $value);
			}

			$processed_headers[] = Text::ucfirst($header).': '.$value;
		}

		if ( ! isset($headers['content-type']))
		{
			$processed_headers[] = 'Content-Type: '.Kohana::$content_type.'; charset='.Kohana::$charset;
		}

		if (Kohana::$expose AND ! isset($headers['x-powered-by']))
		{
			$processed_headers[] = 'X-Powered-By: '.Kohana::version();
		}

		// Get the cookies and apply
		if ($cookies = $response->cookie())
		{
			$processed_headers['Set-Cookie'] = $cookies;
		}

		if (is_callable($callback))
		{
			// Use the callback method to set header
			return call_user_func($callback, $response, $processed_headers, $replace);
		}
		else
		{
			$this->_send_headers_to_php($processed_headers, $replace);
			return $response;
		}
	}

	/**
	 * Sends the supplied headers to the PHP output buffer. If cookies
	 * are included in the message they will be handled appropriately.
	 *
	 * @param   array   $headers    headers to send to php
	 * @param   boolean $replace    replace existing headers
	 * @return  self
	 * @since   3.2.0
	 */
	protected function _send_headers_to_php(array $headers, $replace)
	{
		// If the headers have been sent, get out
		if (headers_sent())
			return $this;

		foreach ($headers as $key => $line)
		{
			if ($key == 'Set-Cookie' AND is_array($line))
			{
				// Send cookies
				foreach ($line as $name => $value)
				{
					Cookie::set($name, $value['value'], $value['expiration']);
				}

				continue;
			}

			header($line, $replace);
		}

		return $this;
	}

} // End Kohana_HTTP_Header
