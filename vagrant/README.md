# Vagrant for FrameworkBenchmarks

Vagrant allows you to create virtual machine that will contain whichever framework you want, ready to test.

Usage:
* install VirtualBox and Vagrant
* `cd vagrant`
* uncomment frameworks to test at the end of puppet.pp (i.e. import "../php-cakephp/puppet.pp")
* `vagrant up`
* wait for virtual machine to boot
* visit framework url (i.e. http://php-cakephp.box.kyon.pl/json.json)

Manual test:
* download and compile wrk (https://github.com/wg/wrk)
* `wrk -t 1 -c 256 -d 15s http://php-cakephp.box.kyon.pl/json.json`

