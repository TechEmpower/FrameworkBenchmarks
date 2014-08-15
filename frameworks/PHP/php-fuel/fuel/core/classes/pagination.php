<?php
/**
 * Part of the Fuel framework.
 *
 * @package    Fuel
 * @version    1.5
 * @author     Fuel Development Team
 * @license    MIT License
 * @copyright  2010 - 2013 Fuel Development Team
 * @link       http://fuelphp.com
 */

namespace Fuel\Core;


class Pagination
{
	/**
	 * @var	array	Pagination instances
	 */
	protected static $_instances = array();

	/**
	 * @var	array	Pagination default instance
	 */
	protected static $_instance = null;

	/**
	 * Init
	 *
	 * Loads in the config and sets the variables
	 *
	 * @return	void
	 */
	public static function _init()
	{
		\Config::load('pagination', true);
		\Lang::load('pagination', true);
	}

	/**
	 * Static access to the default instance
	 *
	 * @return	mixed
	 * @throws	BadMethodCallException if the request method does not exist
	 */
	public static function __callStatic($name, $arguments)
	{
		// old pre-1.4 mapping to new instance methods
		static $mapping = array(
			'get' => '__get',
			'set' => '__set',
			'set_config' => '__set',
			'create_links' => 'render',
			'page_links' => 'pages_render',
			'prev_link' => 'previous',
			'next_link' => 'next',
		);

		array_key_exists($name, $mapping) and $name = $mapping[$name];

		// call the method on the default instance
		if ($instance = static::instance() and method_exists($instance, $name))
		{
			return call_user_func_array(array($instance, $name), $arguments);
		}

		throw new \BadMethodCallException('The pagination class doesn\'t have a method called "'.$name.'"');
	}

	/**
	 * forge a new pagination instance
	 *
	 * @return	\Pagination	a new pagination instance
	 */
	public static function forge($name = 'default', $config = array())
	{
		if ($exists = static::instance($name))
		{
			\Error::notice('Pagination with this name exists already, cannot be overwritten.');
			return $exists;
		}

		static::$_instances[$name] = new static($config);

		if ($name == 'default')
		{
			static::$_instance = static::$_instances[$name];
		}

		return static::$_instances[$name];
	}

	/**
	 * retrieve an existing pagination instance
	 *
	 * @return	\Pagination	a existing pagination instance
	 */
	public static function instance($name = null)
	{
		if ($name !== null)
		{
			if ( ! array_key_exists($name, static::$_instances))
			{
				return false;
			}

			return static::$_instances[$name];
		}

		if (static::$_instance === null)
		{
			static::$_instance = static::forge();
		}

		return static::$_instance;
	}

	// --------------------------------------------------------------------

	/**
	 * instance configuration values
	 */
	protected $config = array(
		'current_page'            => null,
		'offset'                  => 0,
		'per_page'                => 10,
		'total_pages'             => 0,
		'total_items'             => 0,
		'num_links'               => 5,
		'uri_segment'             => 3,
		'show_first'              => false,
		'show_last'               => false,
		'pagination_url'          => null,
	);

	/**
	 * instance template values
	 */
	protected $template = array(
		'wrapper'                 => "<div class=\"pagination\">\n\t{pagination}\n</div>\n",
		'first'                   => "<span class=\"first\">\n\t{link}\n</span>\n",
		'first-link'              => "\t\t<a href=\"{uri}\">{page}</a>\n",
		'previous'                => "<span class=\"previous\">\n\t{link}\n</span>\n",
		'previous-link'           => "\t\t<a href=\"{uri}\">{page}</a>\n",
		'previous-inactive'       => "<span class=\"previous-inactive\">\n\t{link}\n</span>\n",
		'previous-inactive-link'  => "\t\t<a href=\"{uri}\">{page}</a>\n",
		'regular'                 => "<span>\n\t{link}\n</span>\n",
		'regular-link'            => "\t\t<a href=\"{uri}\">{page}</a>\n",
		'active'                  => "<span class=\"active\">\n\t{link}\n</span>\n",
		'active-link'             => "\t\t<a href=\"{uri}\">{page}</a>\n",
		'next'                    => "<span class=\"next\">\n\t{link}\n</span>\n",
		'next-link'               => "\t\t<a href=\"{uri}\">{page}</a>\n",
		'next-inactive'           => "<span class=\"next-inactive\">\n\t{link}\n</span>\n",
		'next-inactive-link'      => "\t\t<a href=\"{uri}\">{page}</a>\n",
		'last'                    => "<span class=\"next\">\n\t{link}\n</span>\n",
		'last-link'               => "\t\t<a href=\"{uri}\">{page}</a>\n",
	);

	/**
	 *
	 */
	public function __construct($config = array())
	{
		// make sure config is an array
		is_array($config) or $config = array('name' => $config);

		// and we have a template name
		array_key_exists('name', $config) or $config['name'] = \Config::get('pagination.active', 'default');

		// merge the config passed with the defined configuration
		$config = array_merge(\Config::get('pagination.'.$config['name'], array()), $config);

		// don't need the template name anymore
		unset($config['name']);

		// update the instance default config with the data passed
		foreach ($config as $key => $value)
		{
			$this->__set($key, $value);
		}
	}

	/**
	 * configuration value getter
	 */
	public function __get($name)
	{
		if (array_key_exists($name, $this->config))
		{
			return $this->config[$name];
		}
		elseif (array_key_exists($name, $this->template))
		{
			return $this->template[$name];
		}
		else
		{
			return null;
		}
	}


	/**
	 * configuration value setter
	 */
	public function __set($name, $value = null)
	{
		if (is_array($name))
		{
			foreach($name as $key => $value)
			{
				$this->__set($key, $value);
			}
		}
		else
		{
			if (array_key_exists($name, $this->config))
			{
				$this->config[$name] = $value;
			}
			elseif (array_key_exists($name, $this->template))
			{
				$this->template[$name] = $value;
			}
		}

		// update the page counters
		$this->_recalculate();
	}

	/**
	 * Creates the pagination markup
	 *
	 * @return	string	Markup for the pagination block
	 */
	public function render()
	{
		// no links if we only have one page
		if ($this->config['total_pages'] == 1)
		{
			return '';
		}

		$html = str_replace(
			'{pagination}',
			$this->first().$this->previous().$this->pages_render().$this->next().$this->last(),
			$this->template['wrapper']
		);

		return $html;
	}

	/**
	 * generate the HTML for the page links only
	 *
	 * @return	string	Markup for page number links
	 */
	public function pages_render()
	{
		// no links if we only have one page
		if ($this->config['total_pages'] == 1)
		{
			return '';
		}

		$html = '';

		// let's get the starting page number, this is determined using num_links
		$start = (($this->config['current_page'] - $this->config['num_links']) > 0) ? $this->config['current_page'] - ($this->config['num_links'] - 1) : 1;

		// let's get the ending page number
		$end = (($this->config['current_page'] + $this->config['num_links']) < $this->config['total_pages']) ? $this->config['current_page'] + $this->config['num_links'] : $this->config['total_pages'];

		for($i = $start; $i <= $end; $i++)
		{
			if ($this->config['current_page'] == $i)
			{
				$html .= str_replace(
				    '{link}',
				    str_replace(array('{uri}', '{page}'), array('#', $i), $this->template['active-link']),
				    $this->template['active']
				);
			}
			else
			{
				$html .= str_replace(
				    '{link}',
				    str_replace(array('{uri}', '{page}'), array($this->_make_link($i), $i), $this->template['regular-link']),
				    $this->template['regular']
				);
			}
		}

		return $html;
	}

	/**
	 * Pagination "First" link
	 *
	 * @param	string $value optional text to display in the link
	 *
	 * @return	string	Markup for the 'first' page number link
	 */
	public function first($marker = '&laquo;&laquo;')
	{
		$html = '';

		if ($this->config['show_first'] and $this->config['total_pages'] > 1 and $this->config['current_page'] > 1)
		{
			$html = str_replace(
				'{link}',
				str_replace(array('{uri}', '{page}'), array($this->_make_link(1), $marker), $this->template['first-link']),
				$this->template['first']
			);
		}

		return $html;
	}

	/**
	 * Pagination "Previous" link
	 *
	 * @param	string $value optional text to display in the link
	 *
	 * @return	string	Markup for the 'previous' page number link
	 */
	public function previous($marker = '&laquo;')
	{
		$html = '';

		if ($this->config['total_pages'] > 1)
		{
			if ($this->config['current_page'] == 1)
			{
				$html = str_replace(
				    '{link}',
				    str_replace(array('{uri}', '{page}'), array('#', $marker), $this->template['previous-inactive-link']),
				    $this->template['previous-inactive']
				);
			}
			else
			{
				$previous_page = $this->config['current_page'] - 1;
				$previous_page = ($previous_page == 1) ? '' : $previous_page;

				$html = str_replace(
				    '{link}',
				    str_replace(array('{uri}', '{page}'), array($this->_make_link($previous_page), $marker), $this->template['previous-link']),
				    $this->template['previous']
				);
			}
		}

		return $html;
	}

	/**
	 * Pagination "Next" link
	 *
	 * @param	string $value optional text to display in the link
	 *
	 * @return	string	Markup for the 'next' page number link
	 */
	public function next($marker = '&raquo;')
	{
		$html = '';

		if ($this->config['total_pages'] > 1)
		{
			if ($this->config['current_page'] == $this->config['total_pages'])
			{
				$html = str_replace(
				    '{link}',
				    str_replace(array('{uri}', '{page}'), array('#', $marker), $this->template['next-inactive-link']),
				    $this->template['next-inactive']
				);
			}
			else
			{
				$next_page = $this->config['current_page'] + 1;

				$html = str_replace(
				    '{link}',
				    str_replace(array('{uri}', '{page}'), array($this->_make_link($next_page), $marker), $this->template['next-link']),
				    $this->template['next']
				);
			}
		}

		return $html;
	}

	/**
	 * Pagination "Last" link
	 *
	 * @param	string $value optional text to display in the link
	 *
	 * @return	string	Markup for the 'last' page number link
	 */
	public function last($marker = '&raquo;&raquo;')
	{
		$html = '';

		if ($this->config['show_last'] and $this->config['total_pages'] > 1 and $this->config['current_page'] != $this->config['total_pages'])
		{
			$html = str_replace(
				'{link}',
				str_replace(array('{uri}', '{page}'), array($this->_make_link($this->config['total_pages']), $marker), $this->template['last-link']),
				$this->template['last']
			);
		}

		return $html;
	}

	/**
	 * Prepares vars for creating links
	 */
	protected function _recalculate()
	{
		// calculate the number of pages
		$this->config['total_pages'] = ceil($this->config['total_items'] / $this->config['per_page']) ?: 1;

		// get the current page number from the URI or the query string
		if (is_string($this->config['uri_segment']))
		{
			$this->config['current_page'] = ($this->config['total_items'] > 0 and $this->config['current_page'] > 1) ? $this->config['current_page'] : \Input::get($this->config['uri_segment'], 1);
		}
		else
		{
			$this->config['current_page'] = ($this->config['total_items'] > 0 and $this->config['current_page'] > 1) ? $this->config['current_page'] : (int) \Request::main()->uri->get_segment($this->config['uri_segment']);
		}

		// make sure the current page is within bounds
		if ($this->config['current_page'] > $this->config['total_pages'])
		{
			$this->config['current_page'] = $this->config['total_pages'];
		}
		elseif ($this->config['current_page'] < 1)
		{
			$this->config['current_page'] = 1;
		}

		// the current page must be zero based so that the offset for page 1 is 0.
		$this->config['offset'] = ($this->config['current_page'] - 1) * $this->config['per_page'];
	}

	/**
	 * Generate a pagination link
	 */
	protected function _make_link($page)
	{
		// make sure we have a valid page number
		empty($page) and $page = 1;

		// construct a pagination url if we don't have one
		if (is_null($this->config['pagination_url']))
		{
			// start with the main uri
			$this->config['pagination_url'] = \Uri::main();
			\Input::get() and $this->config['pagination_url'] .= '?'.http_build_query(\Input::get());
		}

		// was a placeholder defined in the url?
		if (strpos($this->config['pagination_url'], '{page}') === false)
		{
			// break the url in bits so we can insert it
			$url = parse_url($this->config['pagination_url']);

			// parse the query string
			if (isset($url['query']))
			{
				parse_str($url['query'], $url['query']);
			}
			else
			{
				$url['query'] = array();
			}

			// is the page number a URI segment?
			if (is_numeric($this->config['uri_segment']))
			{
				// get the URL segments
				$segs = isset($url['path']) ? explode('/', trim($url['path'], '/')) : array();

				// do we have enough segments to insert? we can't fill in any blanks...
				if (count($segs) < $this->config['uri_segment'] - 1)
				{
					throw new \RuntimeException("Not enough segments in the URI, impossible to insert the page number");
				}

				// replace the selected segment with the page placeholder
				$segs[$this->config['uri_segment'] - 1] = '{page}';
				$url['path'] = '/'.implode('/', $segs);
			}
			else
			{
				// add our placeholder
				$url['query'][$this->config['uri_segment']] = '{page}';
			}

			// re-assemble the url
			$query = empty($url['query']) ? '' : '?'.preg_replace('/%7Bpage%7D/', '{page}', http_build_query($url['query']));
			unset($url['query']);
			empty($url['scheme']) or $url['scheme'] .= '://';
			empty($url['port']) or $url['host'] .= ':';
			$this->config['pagination_url'] = implode($url).$query;
		}

		// return the page link
		return str_replace('{page}', $page, $this->config['pagination_url']);
	}

}
