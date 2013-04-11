# Debugging

Kohana includes several tools to help you debug your application.

The most basic of these is [Debug::vars]. This simple method will display any number of variables, similar to [var_export](http://php.net/var_export) or [print_r](http://php.net/print_r), but using HTML for extra formatting.

    // Display a dump of the $foo and $bar variables
    echo Debug::vars($foo, $bar);

Kohana also provides a method to show the source code of a particular file using [Debug::source].

    // Display this line of source code
    echo Debug::source(__FILE__, __LINE__);

If you want to display information about your application files without exposing the installation directory, you can use [Debug::path]:

    // Displays "APPPATH/cache" rather than the real path
    echo Debug::path(APPPATH.'cache');

If you are having trouble getting something to work correctly, you could check your Kohana logs and your webserver logs, as well as using a debugging tool like [Xdebug](http://www.xdebug.org/).