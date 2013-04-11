# Examples

Here are some "real world" examples of using the database library to construct your queries and use the results.

## Examples of Parameterized Statements

TODO: 4-6 examples of parameterized statements of varying complexity, including a good bind() example.

## Pagination and search/filter

In this example, we loop through an array of whitelisted input fields and for each allowed non-empty value we add it to the search query. We make a clone of the query and then execute that query to count the total number of results. The count is then passed to the [Pagination](../pagination) class to determine the search offset. The last few lines search with Pagination's items_per_page and offset values to return a page of results based on the current page the user is on.

	$query = DB::select()->from('users');
	
	//only search for these fields
	$form_inputs = array('first_name', 'last_name', 'email');
	foreach ($form_inputs as $name) 
	{
		$value = Arr::get($_GET, $name, FALSE);
		if ($value !== FALSE AND $value != '')
		{
			$query->where($name, 'like', '%'.$value.'%');
		}
	}
	
	//copy the query & execute it
	$pagination_query = clone $query;
	$count = $pagination_query->select(DB::expr('COUNT(*)) AS mycount')->execute()->get('mycount');
	
	//pass the total item count to Pagination
	$config = Kohana::$config->load('pagination');
	$pagination = Pagination::factory(array(
		'total_items' => $count,
		'current_page'   => array('source' => 'route', 'key' => 'page'), 
		'items_per_page' => 20,
		'view'           => 'pagination/pretty',
		'auto_hide'      => TRUE,
	));
	$page_links = $pagination->render();
	
	//search for results starting at the offset calculated by the Pagination class
	$query->order_by('last_name', 'asc')
		->order_by('first_name', 'asc')
		->limit($pagination->items_per_page)
		->offset($pagination->offset);
	$results = $query->execute()->as_array();

## Having

TODO: example goes here

[!!]  We could use more examples on this page. 