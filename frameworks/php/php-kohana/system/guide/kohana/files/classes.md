# Classes

TODO: Brief intro to classes.

[Models](mvc/models) and [Controllers](mvc/controllers) are classes as well, but are treated slightly differently by Kohana.  Read their respective pages to learn more.

## Helper or Library?

Kohana 3 does not differentiate between "helper" classes and "library" classes like in previous versions.  They are all placed in the `classes/` folder and follow the same conventions.  The distinction is that in general, a "helper" class is used statically,  (for examples see the [helpers included in Kohana](helpers)), and library classes are typically instantiated and used as objects (like the [Database query builders](../database/query/builder)).  The distinction is not black and white, and is irrelevant anyways, since they are treated the same by Kohana.

## Creating a class

To create a new class, simply place a file in the `classes/` directory at any point in the [Cascading Filesystem](files), that follows the [Class naming conventions](conventions#class-names-and-file-location).  For example, lets create a `Foobar` class.

	// classes/Foobar.php
	
	class Foobar {
		static function magic() {
			// Does something
		}
	}
	
We can now call `Foobar::magic()` any where and Kohana will [autoload](autoloading) the file for us.

We can also put classes in subdirectories.

	// classes/Professor/Baxter.php
	
	class Professor_Baxter {
		static function teach() {
			// Does something
		}
	}
	
We could now call `Professor_Baxter::teach()` any where we want.

For examples of how to create and use classes, simply look at the 'classes' folder in `system` or any module.

## Namespacing your classes

TODO: Discuss namespacing to provide transparent extension functionality in your own classes/modules.
