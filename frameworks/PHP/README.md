# Tricks to writing PHP-based Frameworks

Many servers use the `php`, `php-fpm`, or other binaries. If your
server launches with `sudo` (e.g. `sudo php-fpm`) then you should be 
aware that sudo resets the `$PATH` environment variable, and your 
specific binary may not be the one being used. The solution is to 
always use `sudo <full-path-to-my-binary>`. For example, `cakephp`'s
`bash_profile.sh` sets the variable `$PHP_FPM` to be the full path 
to the `php-fpm` binary that `cakephp` wants, and then uses `sudo $PHP_FPM`
to ensure that the `php-fpm` binary used by sudo is the exact binary 
desired. 

# Dependency Management

Many PHP apps use https://getcomposer.org/ for dependency management.