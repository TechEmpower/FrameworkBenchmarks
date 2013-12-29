
class nginx {
    file { '/etc/nginx/sites-enabled/php-phalcon.conf':
        ensure  =>  present,
        source  =>  '/benchmark_root/php-phalcon/puppet/nginx.conf',
        require =>  Package['nginx'],
        notify  =>  Service['nginx']
    }
}

class php5-fpm {
    file { '/etc/php5/fpm/pool.d/php-phalcon.conf':
        ensure  =>  present,
        source  =>  '/benchmark_root/php-phalcon/puppet/php5-fpm.conf',
        require =>  Package['php5-fpm'],
        notify  =>  Service['php5-fpm']

    }
}

class phalcon {
    file { '/etc/php5/fpm/conf.d/phalcon.ini':
        ensure  =>  present,
        source  =>  '/benchmark_root/php-phalcon/puppet/phalcon.ini',
        require =>  Package['php5-fpm'],
        notify  =>  Service['php5-fpm']
    }
}

include nginx, php5-fpm, phalcon

