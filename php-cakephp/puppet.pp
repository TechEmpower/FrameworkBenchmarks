
class nginx {
    file { '/etc/nginx/sites-enabled/php-cakephp.conf':
        ensure  =>  present,
        source  =>  '/benchmark_root/php-cakephp/puppet/nginx.conf',
        require =>  Package['nginx'],
        notify  =>  Service['nginx']
    }
}

class php5-fpm {
    file { '/etc/php5/fpm/pool.d/php-cakephp.conf':
        ensure  =>  present,
        source  =>  '/benchmark_root/php-cakephp/puppet/php5-fpm.conf',
        require =>  Package['php5-fpm'],
        notify  =>  Service['php5-fpm']

    }
}

include nginx, php5-fpm

