# Cascading Filesystem

The Kohana filesystem is a hierarchy of similar directory structures that cascade. The hierarchy in Kohana (used when a file is loaded by [Kohana::find_file]) is in the following order:

1. **Application Path**  
   Defined as `APPPATH` in `index.php`. The default value is `application`.

2. **Module Paths**  
   This is set as an associative array using [Kohana::modules] in `APPPATH/bootstrap.php`. Each of the values of the array will be searched **in the order that the modules are defined**.

3. **System Path**  
   Defined as `SYSPATH` in `index.php`. The default value is `system`. All of the main or "core" files and classes are defined here.

Files that are in directories higher up the include path order take precedence over files of the same name lower down the order, which makes it is possible to overload any file by placing a file with the same name in a "higher" directory:

![Cascading Filesystem Infographic](cascading_filesystem.png)

This image is only shows certain files, but we can use it to illustrate some examples of the cascading filesystem:

* If Kohana catches an error, it would display the `kohana/error.php` view, So it would call `Kohana::find_file('views', 'kohana/error')`.  This would return `application/views/kohana/error.php` because it takes precidence over `system/views/kohana/error.php`.  By doing this we can change the error view without editing the system folder.

* If we used `View::factory('welcome')` it would call `Kohana::find_file('views','welcome')` which would return `application/views/welcome.php` because it takes precidence over `modules/common/views/welcome.php`.  By doing this, you can overwrite things in a module without editing the modules files.

* If use the Cookie class, [Kohana::auto_load] will call `Kohana::find_file('classes', 'Cookie')` which will return `application/classes/Cookie.php`.  Assuming Cookie extends Kohana_Cookie, the autoloader would then call `Kohana::find_file('classes','Kohana/Cookie')` which will return `system/classes/Kohana/Cookie.php` because that file does not exist anywhere higher in the cascade.  This is an example of [transparent extension](extension).

* If you used `View::factory('user')` it would call `Kohana::find_file('views','user')` which would return `modules/common/views/user.php`.

* If we wanted to change something in `config/database.php` we could copy the file to `application/config/database.php` and make the changes there.  Keep in mind that [config files are merged](files/config#merge) rather than overwritten by the cascade.

## Types of Files

The top level directories of the application, module, and system paths have the following default directories:

classes/
:  All classes that you want to [autoload](autoloading) should be stored here. This includes [controllers](mvc/controllers), [models](mvc/models), and all other classes. All classes must follow the [class naming conventions](conventions#class-names-and-file-location) including matching the case of the class i.e. Kohana_Cookie should be stored in classes/Kohana/Cookie.php and not classes/kohana/cookie.php.

config/
:  Configuration files return an associative array of options that can be loaded using [Kohana::$config]. Config files are merged rather than overwritten by the cascade. See [config files](files/config) for more information.

i18n/
:  Translation files return an associative array of strings. Translation is done using the `__()` method. To translate "Hello, world!" into Spanish, you would call `__('Hello, world!')` with [I18n::$lang] set to "es-es". I18n files are merged rather than overwritten by the cascade. See [I18n files](files/i18n) for more information.

messages/
:  Message files return an associative array of strings that can be loaded using [Kohana::message]. Messages and i18n files differ in that messages are not translated, but always written in the default language and referred to by a single key. Message files are merged rather than overwritten by the cascade. See [message files](files/messages) for more information.

views/
:  Views are plain PHP files which are used to generate HTML or other output. The view file is loaded into a [View] object and assigned variables, which it then converts into an HTML fragment. Multiple views can be used within each other. See [views](mvc/views) for more information.

*other*
:  You can include any other folders in your cascading filesystem.  Examples include, but are not limited to, `guide`, `vendor`, `media`, whatever you want.  For example, to find `media/logo.png` in the cascading filesystem you would call `Kohana::find_file('media','logo','png')`.

## Finding Files

The path to any file within the filesystem can be found by calling [Kohana::find_file]:

    // Find the full path to "classes/Cookie.php"
    $path = Kohana::find_file('classes', 'Cookie');

    // Find the full path to "views/user/login.php"
    $path = Kohana::find_file('views', 'user/login');
	
If the file doesn't have a `.php` extension, pass the extension as the third param.

	// Find the full path to "guide/menu.md"
	$path = Kohana::find_file('guide', 'menu', 'md');

	// If $name is "2000-01-01-first-post" this would look for "posts/2000-01-01-first-post.textile"
	$path = Kohana::find_file('posts', $name, '.textile');


## Vendor Extensions

We call extensions or external libraries that are not specific to Kohana "vendor" extensions, and they go in the vendor folder, either in application or in a module.  Because these libraries do not follow Kohana's file naming conventions, they cannot be autoloaded by Kohana, so you will have to manually included them. Some examples of vendor libraries are [Markdown](http://daringfireball.net/projects/markdown/), [DOMPDF](http://code.google.com/p/dompdf),  [Mustache](http://github.com/bobthecow/mustache.php) and [Swiftmailer](http://swiftmailer.org/).

For example, if you wanted to use [DOMPDF](http://code.google.com/p/dompdf), you would copy it to `application/vendor/dompdf` and include the DOMPDF autoloading class.  It can be useful to do this in a controller's before method, as part of a module's init.php, or the contstructor of a singleton class.

    require Kohana::find_file('vendor', 'dompdf/dompdf/dompdf_config','inc');

Now you can use DOMPDF without loading any more files:

    $pdf = new DOMPDF;

[!!] If you want to convert views into PDFs using DOMPDF, try the [PDFView](http://github.com/shadowhand/pdfview) module.