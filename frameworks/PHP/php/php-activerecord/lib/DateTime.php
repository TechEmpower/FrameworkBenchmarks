<?php
/**
 * @package ActiveRecord
 */
namespace ActiveRecord;

/**
 * An extension of PHP's DateTime class to provide dirty flagging and easier formatting options.
 *
 * All date and datetime fields from your database will be created as instances of this class.
 *
 * Example of formatting and changing the default format:
 *
 * <code>
 * $now = new ActiveRecord\DateTime('2010-01-02 03:04:05');
 * ActiveRecord\DateTime::$DEFAULT_FORMAT = 'short';
 *
 * echo $now->format();         # 02 Jan 03:04
 * echo $now->format('atom');   # 2010-01-02T03:04:05-05:00
 * echo $now->format('Y-m-d');  # 2010-01-02
 *
 * # __toString() uses the default formatter
 * echo (string)$now;           # 02 Jan 03:04
 * </code>
 *
 * You can also add your own pre-defined friendly formatters:
 *
 * <code>
 * ActiveRecord\DateTime::$FORMATS['awesome_format'] = 'H:i:s m/d/Y';
 * echo $now->format('awesome_format')  # 03:04:05 01/02/2010
 * </code>
 *
 * @package ActiveRecord
 * @see http://php.net/manual/en/class.datetime.php
 */
class DateTime extends \DateTime
{
	/**
	 * Default format used for format() and __toString()
	 */
	public static $DEFAULT_FORMAT = 'rfc2822';

	/**
	 * Pre-defined format strings.
	 */
	public static $FORMATS = array(
		'db'      => 'Y-m-d H:i:s',
		'number'  => 'YmdHis',
		'time'    => 'H:i',
		'short'   => 'd M H:i',
		'long'    => 'F d, Y H:i',
		'atom'    => \DateTime::ATOM,
		'cookie'  => \DateTime::COOKIE,
		'iso8601' => \DateTime::ISO8601,
		'rfc822'  => \DateTime::RFC822,
		'rfc850'  => \DateTime::RFC850,
		'rfc1036' => \DateTime::RFC1036,
		'rfc1123' => \DateTime::RFC1123,
		'rfc2822' => \DateTime::RFC2822,
		'rfc3339' => \DateTime::RFC3339,
		'rss'     => \DateTime::RSS,
		'w3c'     => \DateTime::W3C);

	private $model;
	private $attribute_name;

	public function attribute_of($model, $attribute_name)
	{
		$this->model = $model;
		$this->attribute_name = $attribute_name;
	}

	/**
	 * Formats the DateTime to the specified format.
	 *
	 * <code>
	 * $datetime->format();         # uses the format defined in DateTime::$DEFAULT_FORMAT
	 * $datetime->format('short');  # d M H:i
	 * $datetime->format('Y-m-d');  # Y-m-d
	 * </code>
	 *
	 * @see FORMATS
	 * @see get_format
	 * @param string $format A format string accepted by get_format()
	 * @return string formatted date and time string
	 */
	public function format($format=null)
	{
		return parent::format(self::get_format($format));
	}

	/**
	 * Returns the format string.
	 *
	 * If $format is a pre-defined format in $FORMATS it will return that otherwise
	 * it will assume $format is a format string itself.
	 *
	 * @see FORMATS
	 * @param string $format A pre-defined string format or a raw format string
	 * @return string a format string
	 */
	public static function get_format($format=null)
	{
		// use default format if no format specified
		if (!$format)
			$format = self::$DEFAULT_FORMAT;

		// format is a friendly
		if (array_key_exists($format, self::$FORMATS))
			 return self::$FORMATS[$format];

		// raw format
		return $format;
	}

	public function __toString()
	{
		return $this->format();
	}

	private function flag_dirty()
	{
		if ($this->model)
			$this->model->flag_dirty($this->attribute_name);
	}

	public function setDate($year, $month, $day)
	{
		$this->flag_dirty();
		call_user_func_array(array($this,'parent::setDate'),func_get_args());
	}

	public function setISODate($year, $week , $day=null)
	{
		$this->flag_dirty();
		call_user_func_array(array($this,'parent::setISODate'),func_get_args());
	}

	public function setTime($hour, $minute, $second=null)
	{
		$this->flag_dirty();
		call_user_func_array(array($this,'parent::setTime'),func_get_args());
	}

	public function setTimestamp($unixtimestamp)
	{
		$this->flag_dirty();
		call_user_func_array(array($this,'parent::setTimestamp'),func_get_args());
	}
}
?>
