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

import "manifests/nginx.pp"
import "manifests/php5-fpm.pp"
import "manifests/mysql.pp"


# PHP frameworks
import "/benchmark_root/php/puppet.pp"
import "/benchmark_root/php-cakephp/puppet.pp"
import "/benchmark_root/php-silex/puppet.pp"
import "/benchmark_root/php-phalcon/puppet.pp"
