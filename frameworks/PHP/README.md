# Tricks to writing PHP-based Frameworks

Many servers use the `php`, `php-fpm`, or other binaries. If your
server launches with `sudo` (e.g. `sudo php-fpm`) then you should be 
aware that using sudo resets the `$PATH` environment variable, and your 
specific binary may not be the one being used. The solution is to 
always use `sudo <full-path-to-my-binary>`. For example, `cakephp`'s
`bash_profile.sh` sets the variable `$PHP_FPM` to be the full path 
to the `php-fpm` binary that `cakephp` wants, and then uses `sudo $PHP_FPM`
to ensure that the `php-fpm` binary used by sudo is the exact binary 
desired. 

# Dependency Management Using Composer

Many PHP apps use [Composer](https://getcomposer.org/) for dependency management.
To support this, setup your `install.sh` with these two commands

    # Note the order! Composer depends on PHP so it has to come second
    fw_depends php composer 
    
    # Download dependencies
    ${IROOT}/php-5.5.17/bin/php $IROOT/composer.phar install \
    --no-interaction --working-dir $TROOT --no-progress \
    --optimize-autoloader 

Add your `composer.json` file to your framework's folder, e.g. `php-fuel/composer.json`. 
After installation runs, your framework folder will have a new `vendor` folder, 
e.g. `php-fuel/vendor` that contains all dependencies. 

## One-time Composer Setup

Composer uses Github *a lot*, enough so that it is common 
for it to exceed the API limit and cause infinite hangs or 
installation failures. To avoid this, it is necessary to 
generate a `composer.lock` file by manually running the 
installation one time - this file will list the exact 
github URLs that are needed for this project, which is then
used by subsequent composer commands to avoid Github's 
rate limits. More on this [here](https://getcomposer.org/doc/03-cli.md#install) and [here](https://circleci.com/docs/composer-api-rate-limit)

You need to generate `composer.lock` manually, and then add it 
to your framework's folder. Use these steps

    # Run one installation to ensure PHP is installed
    cd FrameworkBenchmarks
    toolset/run-tests.py --install server --install-only --test <your test>
     
    # Switch to the user that runs tests
    sudo su testrunner
    
    # Define the environment variables you need (modify as needed)
    export IROOT=/home/you/FrameworkBenchmarks/installs
    export TROOT=/home/you/FrameworkBenchmarks/frameworks/PHP/php-yii2
    
    # Run the installation shown above
    #
    # This will manually prompt you for your Github credentials 
    # to avoid the Github rate limit, and when this command
    # finishes running you will have a new file composer.lock
    ${IROOT}/php-5.5.17/bin/php $IROOT/composer.phar install \
      --working-dir $TROOT
    
    # Add the lock file to this repository so it's used for all 
    # future installations
    git add -f composer.lock

**NOTE:** You should run this process manually once to generate the `composer.lock` file



php), and then add `$IROOT/php-composer` to the `PATH` in your `bash_profile.sh`. 
For example: 

    export COMPOSER_HOME=${IROOT}/php-composer
    export PATH="$COMPOSER_HOME:$PATH"


# Debugging PHP Frameworks

The first stop for HTTP 500 errors is to enable stack 
traces. Update `config/php-fpm.conf` to 
include `php_flag[display_errors] = on`. 
