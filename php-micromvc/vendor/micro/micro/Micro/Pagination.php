<?php
/**
 * Pagination
 *
 * Provides HTML Pagination links for large datasets
 *
 * @package		MicroMVC
 * @author		David Pennington
 * @copyright	(c) 2011 MicroMVC Framework
 * @license		http://micromvc.com/license
 ********************************** 80 Columns *********************************
 */
namespace Micro;

class Pagination
{

	public $total = NULL;
	public $current = NULL;
	public $path = NULL;
	public $per_page = NULL;
	public $params = NULL;
	public $links = 2;
	public $attributes = array('class' => 'pagination', 'id' => 'pagination');


	/**
	 * Creates pagination links for the total number of pages
	 *
	 * @param int $total number of items
	 * @param int $current page
	 * @param int $per_page the number to show per-page (default 10)
	 * @param string $path to place in the links
	 * @return string
	 */
	public function __construct($total, $current, $per_page = 10, $path = NULL, $params = NULL)
	{
		$this->current = (int) $current;
		$this->per_page = (int) $per_page;
		$this->total = (int) ceil($total / $per_page);
		$this->path = $path;

		// Assume the current URL parameters if not given
		if($params === NULL)
		{
			$this->params = $_GET;
		}
		elseif($params)
		{
			$this->params = $params;
		}
	}


	/**
	 * Create a "previous page" link if needed
	 *
	 * @return string
	 */
	public function previous()
	{
		if($this->current > 1)
		{
			return HTML::tag('li', HTML::link($this->url($this->current-1), _('&larr; Previous'), array('class' => 'previous')));
		}
	}


	/**
	 * Create a "first page" link if needed
	 *
	 * @return string
	 */
	public function first()
	{
		if($this->current > $this->links + 1)
		{
			return HTML::tag('li', HTML::link($this->url(1), _('&lt; First')), array('class' => 'first'));
		}
	}


	/**
	 * Create a "last page" link if needed
	 *
	 * @return string
	 */
	public function last()
	{
		if($this->current + $this->links  < $this->total)
		{
			return HTML::tag('li', HTML::link($this->url($this->total), _('Last &gt;')), array('class' => 'last'));
		}
	}


	/**
	 * Create a "next page" link if needed
	 *
	 * @return string
	 */
	public function next()
	{
		$attributes = array('class' => 'next');

		if($this->total < 2 OR $this->current < $this->total)
		{
			$attributes = array('class' => 'disabled next');
		}

		return HTML::tag('li', HTML::link($this->url($this->current+1), _('Next &rarr;')), $attributes);
	}



	/**
	 * Return an HTML pagination string
	 *
	 * @return string
	 */
	public function __toString()
	{
		try
		{
			// Start and end must be valid integers
			$start = (($this->current - $this->links) > 0) ? $this->current - $this->links : 1;
			$end = (($this->current + $this->links) < $this->total) ? $this->current + $this->links : $this->total;

			$html = $this->previous();

			for($i = $start; $i <= $end; ++$i)
			{
				// Current link is "active"
				$attributes = $this->current == $i ? array('class' => 'active') : array();

				// Wrap the link in a list item
				$html .= HTML::tag('li', HTML::link($this->url($i), $i), $attributes);
			}

			$html .= $this->next();

			return HTML::tag('div', "<ul>\n" . $html . "</ul>\n", $this->attributes);
		}
		catch(\Exception $e)
		{
			Error::exception($e);
			return '';
		}

	}


	/**
	 * Build the pagination URL
	 *
	 * @param integer $page number
	 */
	public function url($page = NULL)
	{
		return site_url($this->path, (array) $this->params + array('page' => $page));
	}
}

// END
