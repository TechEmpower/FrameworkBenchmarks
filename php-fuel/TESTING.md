# Testing FuelPHP

FuelPHP uses [PHPUnit](https://github.com/sebastianbergmann/phpunit/) for it's Unit Testing needs. It must be installed for the tests to run.

**NOTE: No code will be accepted without tests written.**

## Running Tests

Running the unit tests is as simple as navigating to the root install folder on the command line and running the following:

    $ php oil test

That's it! You can also tell it specific groups (which we will get into in minute) to run. For example to run only the core tests:

    $ php oil test --group=Core

As you can see we've wrapped the phpunit command with our own Oil utility which will in time become more and more useful. If you wish to get right at the phpunit tests manually you can point it to our .xml configuration file:

	$ phpunit -c fuel/core/phpunit.xml --group Core

This may break or change in future versions so it's suggested you stick with using Oil for your testing.

## Writing Tests

### Where do they go?

All tests are to go in the **tests** folders inside their respective parent folder.  For instance:

* App tests go in *fuel/app/tests*
* Core tests go in *fuel/core/tests*
* Package tests go in *fuel/packages/package_name/tests*

### File / Class Naming

The Test class names should be in the form of **Tests_Whatever**.

Filenames should be all lower case and be the class name minus the "Tests_" part.

Some example names:

    // Good
    Tests_Arr in fuel/core/tests/arr.php
    Tests_Image in fuel/core/tests/image.php
    Tests_Fuel in fuel/core/tests/fuel.php

    // Bad
    Arrtests
    Somestuff

### Test Grouping

All tests inside the **core** folder must be in the **core** group.  A classes test's should also be grouped together under the name of the class.

Here is an example of a core class test with proper DocBlocks:

    /**
     * Arr class tests
     *
     * @group Core
     * @group Arr
     */
    class Tests_Arr extends TestCase {

    	/**
    	 * Tests Arr::get()
    	 *
    	 * @test
    	 */
    	public function test_get()
    	{
    		// Test code here
    	}
    }

All App tests should be in the **app** group.

### Namespaces

All **core** tests should be in the **Fuel\Core** namespace.  This is so that we are sure we are testing the core classes,
not any extensions that may be in *app*.

App tests can be in any namespace.

### What class do I extend?

All tests should extend the **Fuel\Core\TestCase** class.

**NOTE: if you are in the Fuel\Core namespace you can leave off the Fuel\Core namespace and just extend **TestCase**.

## Example

    namespace Fuel\Core;

    /**
     * Arr class tests
     *
     * @group Core
     * @group Arr
     */
    class Tests_Arr extends TestCase {

    	/**
    	 * Tests Arr::flatten_assoc()
    	 *
    	 * @test
    	 */
    	public function test_flatten_assoc()
    	{
    		$people = array(
    			array(
    				"name" => "Jack",
    				"age" => 21
    			),
    			array(
    				"name" => "Jill",
    				"age" => 23
    			)
    		);

    		$expected = array(
    			"0:name" => "Jack",
    			"0:age" => 21,
    			"1:name" => "Jill",
    			"1:age" => 23
    		);

    		$output = Arr::flatten_assoc($people);
    		$this->assertEquals($expected, $output);
    	}

    }

