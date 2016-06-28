<?php
/**
 * Table
 *
 * Create sortable HTML tables from result sets.
 *
 * @package		MicroMVC
 * @author		David Pennington
 * @copyright	(c) 2011 MicroMVC Framework
 * @license		http://micromvc.com/license
 ********************************** 80 Columns *********************************
 */
namespace Micro;

class Table
{
	// Array of data rows
	public $rows;

	// List of all table columns
	public $columns;

	// Ordering parameters
	public $column;
	public $sort;

	// Existing parameters
	public $params;

	// Table ID, class, etc
	public $attributes;

	/**
	 * Create the table object using these rows
	 *
	 * @param array $rows to use
	 */
	public function __construct($rows)
	{
		$this->rows = $rows;

		// Set order defaults
		$this->params = $_GET;
		$this->column = get('column');
		$this->sort = get('sort', 'asc');
		$this->attributes = array('class' => 'table');
	}


	/**
	 * Add a new field to the validation object
	 *
	 * @param string $field name
	 */
	public function column($header, $name, $function = NULL)
	{
		$this->columns[$header] = array($name, $function);

		return $this;
	}


	public function render()
	{
		$html = "\n\t<thead>\n\t\t<tr>";

		foreach($this->columns as $header => $data)
		{
			$html .= "\n\t\t\t<th>";

			// If we allow sorting by this column
			if($data[0])
			{
				// If this column matches the current sort column - go in reverse
				if($this->column === $data[0])
				{
					$sort = $this->sort == 'asc' ? 'desc' : 'asc';
				}
				else
				{
					$sort = $this->sort == 'asc' ? 'asc' : 'desc';
				}

				// Build URL parameters taking existing parameters into account
				$url = site_url(NULL, array('column' => $data[0], 'sort' => $sort) + $this->params);

				$html .= '<a href="' . $url . '" class="table_sort_' . $sort . '">' . $header . '</a>';
			}
			else
			{
				$html .= $header;
			}

			$html .= "</th>";
		}

		$html .= "\n\t\t</tr>\n\t</thead>\n\t<tbody>";

		$odd = 0;
		foreach($this->rows as $row)
		{
			$odd = 1 - $odd;

			$html .= "\n\t\t<tr class=\"". ($odd ? 'odd' : 'even') . '">';
			foreach($this->columns as $header => $data)
			{
				if($data[1])
				{
					$html .= "\n\t\t\t<td>" . $data[1]($row) . "</td>";
				}
				else
				{
					$html .= "\n\t\t\t<td>" . $row->$data[0] . "</td>";
				}
			}
			$html .= "\n\t\t</tr>";
		}

		$html .= "\n\t</tbody>\n";

		return HTML::tag('table', $html, $this->attributes);
	}


	/**
	 * alias for render()
	 *
	 * @return string
	 */
	public function __toString()
	{
		try
		{
			return $this->render();
		}
		catch(\Exception $e)
		{
			Error::exception($e);
			return '';
		}
	}
}

// END
