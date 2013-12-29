class baseconfig {
    exec { 'apt-get update':
        command =>  '/usr/bin/apt-get update'
    }
}

stage { 'pre':
    before  =>  Stage['main']
}
class {
    'baseconfig':
        stage => 'pre'
}

# PHP frameworks
import "/benchmark_root/php-cakephp/puppet.pp"
