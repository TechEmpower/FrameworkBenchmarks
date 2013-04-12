# Upgrading

## Table aliases

ORM [will now alias the main table](http://dev.kohanaframework.org/issues/4066) in a query to the model's singular object name.
i.e. Prior to 3.2 ORM set the from table like so:

	$this->_db_builder->from($this->_table_name);

As of 3.2 it is now aliased like so:

	$this->_db_builder->from(array($this->_table_name, $this->_object_name));

If you have a model `Model_Order` then when building a query use the alias like so:

	$model->where('order.id', '=', $id);
