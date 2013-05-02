# Installation

1. Download the latest **stable** release from the [Kohana website](http://kohanaframework.org/).
2. Unzip the downloaded package to create a `kohana` directory.
3. Upload the contents of this folder to your webserver.
4. Open `application/bootstrap.php` and make the following changes:
	- Set the default [timezone](http://php.net/timezones) for your application.
		
		~~~
			// Example of changing timezone from Chicago to Sao Paulo, Brazil
			date_default_timezone_set('America/Sao_Paulo');
		~~~
	- Set the `base_url` in the [Kohana::init] call to reflect the location of the kohana folder on your server relative to the document root.

		~~~
			// Example of kohana's installation at /var/www/mywebsite and
			// Apache's DocumentRoot configured to /var/www
			Kohana::init(array(
				'base_url'   => '/mywebsite',
			));
		~~~	
6. Make sure the `application/cache` and `application/logs` directories are writable by the web server.

	~~~
		sudo chmod 777 -R application/cache
		sudo chmod 777 -R application/logs
	~~~
	
7. Test your installation by opening the URL you set as the `base_url` in your favorite browser.

[!!] Depending on your platform, the installation's subdirs may have lost their permissions thanks to zip extraction. Chmod them all to 755 by running `find . -type d -exec chmod 0755 {} \;` from the root of your Kohana installation.

You should see the installation page. If it reports any errors, you will need to correct them before continuing.

![Install Page](install.png "Example of install page")

Once your install page reports that your environment is set up correctly you need to either rename or delete `install.php` in the root directory. Kohana is now installed and you should see the output of the welcome controller:

![Welcome Page](welcome.png "Example of welcome page")

## Installing Kohana From GitHub

The [source code](http://github.com/kohana/kohana) for Kohana is hosted with [GitHub](http://github.com).  To install Kohana using the github source code first you need to install [git](http://git-scm.com/).  Visit [http://help.github.com](http://help.github.com) for details on how to install git on your platform.

[!!] For more information on installing Kohana using git submodules, see the [Working with Git](tutorials/git) tutorial.