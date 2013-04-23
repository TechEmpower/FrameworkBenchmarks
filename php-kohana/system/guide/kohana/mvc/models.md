# Models

From Wikipedia:

 > The model manages the behavior and data of the application domain,
 > responds to requests for information about its state (usually from the view),
 > and responds to instructions to change state (usually from the controller).

Creating a simple model:

	class Model_Post extends Model
	{
		public function do_stuff()
		{
			// This is where you do domain logic...
		}
	}

If you want database access, have your model extend the Model_Database class:

	class Model_Post extends Model_Database
	{
		public function do_stuff()
		{
			// This is where you do domain logic...
		}

		public function get_stuff()
		{
			// Get stuff from the database:
			return $this->db->query(...);
		}
	}

If you want CRUD/ORM capabilities, see the [ORM Module](../../guide/orm)