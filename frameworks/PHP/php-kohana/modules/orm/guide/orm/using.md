# Basic Usage

## Load a new model instance

To create a new `Model_User` instance, you can do one of two things:

	$user = ORM::factory('User');
	// Or
	$user = new Model_User();

## Inserting

To insert a new record into the database, create a new instance of the model:

	$user = ORM::factory('User');

Then, assign values for each of the properties;

	$user->first_name = 'Trent';
	$user->last_name = 'Reznor';
	$user->city = 'Mercer';
	$user->state = 'PA';

Insert the new record into the database by running [ORM::save]:

	$user->save();

[ORM::save] checks to see if a value is set for the primary key (`id` by default). If the primary key is set, then ORM will execute an `UPDATE` otherwise it will execute an `INSERT`.


## Finding an object

To find an object you can call the [ORM::find] method or pass the id into the ORM constructor:

	// Find user with ID 20
	$user = ORM::factory('User')
		->where('id', '=', 20)
		->find();
	// Or
	$user = ORM::factory('User', 20);

## Check that ORM loaded a record

Use the [ORM::loaded] method to check that ORM successfully loaded a record.

	if ($user->loaded())
	{
		// Load was successful
	}
	else
	{
		// Error
	}

## Updating and Saving

Once an ORM model has been loaded, you can modify a model's properties like this:

	$user->first_name = "Trent";
	$user->last_name = "Reznor";

And if you want to save the changes you just made back to the database, just run a `save()` call like this:

	$user->save();



## Deleting


To delete an object, you can call the [ORM::delete] method on a loaded ORM model.

	$user = ORM::factory('User', 20);
	$user->delete();

	
## Mass assignment
	

To set multiple values at once, use [ORM::values]
	
	try
	{
		$user = ORM::factory('user')
			->values($this->request->post(), array('username','password'))
			->create();
	}
	catch (ORM_Validation_Exception $e)
	{
		// Handle validation errors ...
	}
	
[!!] Although the second argument is optional, it is *highly recommended* to specify the list of columns you expect to change. Not doing so will leave your code _vulnerable_ in case the attacker adds fields you didn't expect.

