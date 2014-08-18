# Validation

*This page needs to be reviewed for accuracy by the development team. Better examples would be helpful.*

Validation can be performed on any array using the [Validation] class. Labels and rules can be attached to a Validation object by the array key, called a "field name".

labels
:  A label is a human-readable version of the field name.

rules
:  A rule is a callback or closure used to decide whether or not to add an error to a field

[!!] Note that any valid [PHP callback](http://php.net/manual/language.pseudo-types.php#language.types.callback) can be used as a rule.

Using `TRUE` as the field name when adding a rule will be applied to all named fields.

Creating a validation object is done using the [Validation::factory] method:

    $object = Validation::factory($array);

[!!] The `$object` object will be used for the rest of this tutorial. This tutorial will show you how to validate the registration of a new user.

## Provided Rules

Kohana provides a set of useful rules in the [Valid] class:

Rule name                 | Function
------------------------- |-------------------------------------------------
[Valid::not_empty]     | Value must be a non-empty value
[Valid::regex]         | Match the value against a regular expression
[Valid::min_length]    | Minimum number of characters for value
[Valid::max_length]    | Maximum number of characters for value
[Valid::exact_length]  | Value must be an exact number of characters
[Valid::email]         | An email address is required
[Valid::email_domain]  | Check that the domain of the email exists
[Valid::url]           | Value must be a URL
[Valid::ip]            | Value must be an IP address
[Valid::phone]         | Value must be a phone number
[Valid::credit_card]   | Require a credit card number
[Valid::date]          | Value must be a date (and time)
[Valid::alpha]         | Only alpha characters allowed
[Valid::alpha_dash]    | Only alpha and hyphens allowed
[Valid::alpha_numeric] | Only alpha and numbers allowed
[Valid::digit]         | Value must be an integer digit
[Valid::decimal]       | Value must be a decimal or float value
[Valid::numeric]       | Only numeric characters allowed
[Valid::range]         | Value must be within a range
[Valid::color]         | Value must be a valid HEX color
[Valid::matches]       | Value matches another field value

## Adding Rules

All validation rules are defined as a field name, a method, a function (using the [PHP callback](http://php.net/callback) syntax) or [closure](http://php.net/manual/functions.anonymous.php), and an array of parameters:

    $object->rule($field, $callback, array($parameter1, $parameter2));

If no parameters are specified, the field value will be passed to the callback. The following two rules are equivalent:

    $object->rule($field, 'not_empty');
    $object->rule($field, 'not_empty', array(':value'));

Rules defined in the [Valid] class can be added by using the method name alone. The following three rules are equivalent:

    $object->rule('number', 'phone');
    $object->rule('number', array('Valid', 'phone'));
    $object->rule('number', 'Valid::phone');

### Adding Rules for multiple fields together

To validate multiple fields together, you can do something like this:

    $object->rule('one', 'only_one', array(':validation', array('one', 'two')));
    $object->rule('two', 'only_one', array(':validation', array('one', 'two')));

    public function only_one($validation, $fields)
    {
        // If more than 1 field is set, bail.
        $matched = 0;

        foreach ($fields as $field)
        {
            if (isset($validation[$field]))
            {
                $matched++;
            }
        }

        if ($matched > 0)
        {
            // Add the error to all concerned fields
            foreach ($fields as $field)
            {
                $validation->error($field, 'only_one');
            }
        }
    }

## Binding Variables

The [Validation] class allows you to bind variables to certain strings so that they can be used when defining rules. Variables are bound by calling the [Validation::bind] method.

    $object->bind(':model', $user_model);
    // Future code will be able to use :model to reference the object
    $object->rule('username', 'some_rule', array(':model'));

By default, the validation object will automatically bind the following values for you to use as rule parameters:

- `:validation` - references the validation object
- `:field` - references the field name the rule is for
- `:value` - references the value of the field the rule is for

## Adding Errors

The [Validation] class will add an error for a field if any of the rules associated to it return `FALSE`. This allows many built in PHP functions to be used as rules, like `in_array`.

    $object->rule('color', 'in_array', array(':value', array('red', 'green', 'blue')));

Rules added to empty fields will run, but returning `FALSE` will not automatically add an error for the field. In order for a rule to affect empty fields, you must add the error manually by calling the [Validation::error] method. In order to do this, you must pass the validation object to the rule.

    $object->rule($field, 'the_rule', array(':validation', ':field'));

    public function the_rule($validation, $field)
    {
        if (something went wrong)
        {
            $validation->error($field, 'the_rule');
        }
    }

[!!] `not_empty` and `matches` are the only rules that will run on empty fields and add errors by returning `FALSE`.

## Example

To start our example, we will perform validation on the HTTP POST data of the current request that contains user registration information:

[!!] In Kohana controllers, we access `$this->request->post()` instead of `$_POST` for better request isolation.

    $object = Validation::factory($this->request->post());

Next we need to process the POST'ed information using [Validation]. To start, we need to add some rules:

    $object
        ->rule('username', 'not_empty')
        ->rule('username', 'regex', array(':value', '/^[a-z_.]++$/iD'))
        ->rule('password', 'not_empty')
        ->rule('password', 'min_length', array(':value', '6'))
        ->rule('confirm',  'matches', array(':validation', 'confirm', 'password'))
        ->rule('use_ssl', 'not_empty');

Any existing PHP function can also be used a rule. For instance, if we want to check if the user entered a proper value for the SSL question:

    $object->rule('use_ssl', 'in_array', array(':value', array('yes', 'no')));

Note that all array parameters must still be wrapped in an array! Without the wrapping array, `in_array` would be called as `in_array($value, 'yes', 'no')`, which would result in a PHP error.

Any custom rules can be added using a [PHP callback](http://php.net/manual/language.pseudo-types.php#language.types.callback]:

    $object->rule('username', 'User_Model::unique_username');

The method `User_Model::unique_username()` would be defined similar to:

    public static function unique_username($username)
    {
        // Check if the username already exists in the database
        return ! DB::select(array(DB::expr('COUNT(username)'), 'total'))
            ->from('users')
            ->where('username', '=', $username)
            ->execute()
            ->get('total');
    }

[!!] Custom rules allow many additional checks to be reused for multiple purposes. These methods will almost always exist in a model, but may be defined in any class.

# A Complete Example

First, we need a [View] that contains the HTML form, which will be placed in `application/views/user/register.php`:

    <?php echo Form::open() ?>
    <?php if ($errors): ?>
    <p class="message">Some errors were encountered, please check the details you entered.</p>
    <ul class="errors">
    <?php foreach ($errors as $message): ?>
        <li><?php echo $message ?></li>
    <?php endforeach ?>
    <?php endif ?>

    <dl>
        <dt><?php echo Form::label('username', 'Username') ?></dt>
        <dd><?php echo Form::input('username', $post['username']) ?></dd>

        <dt><?php echo Form::label('password', 'Password') ?></dt>
        <dd><?php echo Form::password('password') ?></dd>
        <dd class="help">Passwords must be at least 6 characters long.</dd>
        <dt><?php echo Form::label('confirm', 'Confirm Password') ?></dt>
        <dd><?php echo Form::password('confirm') ?></dd>

        <dt><?php echo Form::label('use_ssl', 'Use extra security?') ?></dt>
        <dd><?php echo Form::select('use_ssl', array('yes' => 'Always', 'no' => 'Only when necessary'), $post['use_ssl']) ?></dd>
        <dd class="help">For security, SSL is always used when making payments.</dd>
    </dl>

    <?php echo Form::submit(NULL, 'Sign Up') ?>
    <?php echo Form::close() ?>

[!!] This example uses the [Form] helper extensively. Using [Form] instead of writing HTML ensures that all of the form inputs will properly handle input that includes HTML characters. If you prefer to write the HTML yourself, be sure to use [HTML::chars] to escape user input.

Next, we need a controller and action to process the registration, which will be placed in `application/classes/Controller/User.php`:

    class Controller_User extends Controller {

        public function action_register()
        {
            $user = Model::factory('user');

            $validation = Validation::factory($this->request->post())
                ->rule('username', 'not_empty')
                ->rule('username', 'regex', array(':value', '/^[a-z_.]++$/iD'))
                ->rule('username', array($user, 'unique_username'))

                ->rule('password', 'not_empty')
                ->rule('password', 'min_length', array(':value', 6))
                ->rule('confirm',  'matches', array(':validation', ':field', 'password'))

                ->rule('use_ssl', 'not_empty')
                ->rule('use_ssl', 'in_array', array(':value', array('yes', 'no')));

            if ($validation->check())
            {
                // Data has been validated, register the user
                $user->register($this->request->post());

                // Always redirect after a successful POST to prevent refresh warnings
                $this->redirect('user/profile', 302);
            }

            // Validation failed, collect the errors
            $errors = $validation->errors('user');

            // Display the registration form
            $this->response->body(View::factory('user/register'))
                ->bind('post', $this->request->post())
                ->bind('errors', $errors);
        }

    }

We will also need a user model, which will be placed in `application/classes/Model/User.php`:

    class Model_User extends Model {

        public function register($array)
        {
            // Create a new user record in the database
            $id = DB::insert(array_keys($array))
                ->values($array)
                ->execute();

            // Save the new user id to a cookie
            cookie::set('user', $id);

            return $id;
        }

    }

That is it, we have a complete user registration example that properly checks user input!
