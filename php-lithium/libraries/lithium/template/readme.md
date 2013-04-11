#### Special syntax

Views have a special syntax for outputting escaped text. The standard way to
output escaped text in your views from Lithium is as follows: {{{
<?=$variable; ?>
}}}

This is where a lot of confusion comes in, because it is commonly misunderstood
that Lithium depends on `short_open_tags`, however, that's not the case. The
contents of a view are processed through a [ tokenizer](template/view/Compiler) before
it is included by PHP. The file is then `compiled` into the final PHP+HTML (or whatever
other content type that is requested), which is then passed off to be fully rendered
by the two-step view to its final form.

See the PHP [ manual](http://php.net/manual/en/book.tokenizer.php) to learn more about tokens.

The stream wrapper reads the file and searches for anything that looks like
`<?=...?>` and replaces it with `<?php echo $h(...); ?>`.

The design decision behind using PHP's short echo syntax is because it's a
familiar syntax and it helps developers focus more on what data _should not_ be
escaped vs. what data _needs_ to be escaped.

One special case situation to take _important_ note of, is the use of
`<?=$this->foo(); ?>`. In this scenario, the code is translated to
`<?php echo $this->foo(); ?>` rather than being filtered through `$h()` as with
the former explanation. When direct access to a method or property on `$this` is
contained in the shorthands syntax, it will be output as normal without being
filtered. This is to make it easier to work with helpers that return markup.

An example would be something like: {{{
<?=$this->form->create(); ?>
... my form here ...
<?=$this->form->end(); ?>
}}}

**Note:** `$h()` is the HTML escape function used in views.

**Note:** To output regular, unescaped text, use plain old `<?php echo ...; ?>`.

**Other useful information:**

 - [Introduction to PHP streams](http://www.php.net/intro.stream)
 - [Stream examples](http://www.php.net/stream.examples)

#### Using helpers

Helpers are lazy-loaded by the current renderer. To use a helper, you can
reference it by its name like this: {{{
echo $this->html->link('Example', 'http://www.example.com');
}}}

In a template, `$this` refers to the `Renderer` object. By using `$this->html`
for the first time, the renderer will create an instance of the helper and store
it so that the next time the helper is invoked the renderer will not have to
re-instantiate the helper.

Using such an approach, helpers can easily be loaded as needed without any
performance impact.

**More info**

 - [ HTML helper](template/helper/Html)
 - [ Form helper](template/helper/Form)
 - [ Helper base class](template/Helper)

#### Creating custom helpers

You can also create your own custom helper very easily by extending the `Helper` base class, and
placing your helper in the correct namespace. By default, helpers belong in the
`<library>\extensions\helper` namespace, but this can be changed through configuration (see the
[the `Libraries` class](core/Libraries)). For example, consider the following class, saved as
`extensions/helper/Custom.php`:
{{{
<?php

namespace app\extensions\helper;

class Custom extends \lithium\template\Helper {

	public function greeting($name) {
		return "Hello {$name}!";
	}
}

?>
}}}

You can then use your helper in templates as follows:
{{{
<?=$this->custom->greeting('World'); ?>
}}}

Your custom helper will then be auto-loaded into the templating engine from your application or a
plugin.

#### Extending core helpers

Because your application and plugins have a higher order-of-precedence than the Lithium core,
classes like helpers can be extended and replaced seamlessly, without any changes to your templates.

For example, to add or replace methods in the `Form` helper, you can add the following to
`extensions/helper/Form.php`:

{{{
<?php

namespace app\extensions\helper;

class Form extends \lithium\template\helper\Form {

	// Add or override Form helper methods
}

?>
}}}

Your custom `Form` helper will now be invoked in all instances where `$this->form` is called in a
template. For more information on the load order of classes, see
[the `locate()` method of the `Libraries` class](core/Libraries::locate)

#### Rendering elements

Elements are reusable view snippets that you can use in several views and layouts.
You can reference it like so:
{{{
echo $this->_render('element', 'menu');
}}}

Where `menu` is the name of your element file, in this example `views/elements/menu.html.php`.  When using `$this->_render()`, all of the variables set in the controller are available to the element template.  You can pass variables declared in the view or additional static content using the third parameter to `$this->_render()`:
{{{
$var1 = 'something';
echo $this->_render('element', 'menu', array(
	'var1' => $var1,
	'var2' => 'something else'
));
}}}

If you need the element template to not have access to existing data passed to the parent template, use the alternate syntax that calls the `View` render method directly:
{{{
echo $this->view()->render(
	array('element' => 'menu'),
	array('var1' => $var1, 'var2' => $var2)
);
}}}

**More info**

 - [ View](template/View)
 - [ Renderer](template/view/Renderer)
 - [ File adapter](template/view/adapter/File)