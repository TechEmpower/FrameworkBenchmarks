
class nginx {
    file { '/etc/nginx/sites-enabled/php-silex.conf':
        ensure  =>  present,
        source  =>  '/benchmark_root/php-silex/puppet/nginx.conf',
        require =>  Package['nginx'],
        notify  =>  Service['nginx']
    }
}

class php5-fpm {
    file { '/etc/php5/fpm/pool.d/php-silex.conf':
        ensure  =>  present,
        source  =>  '/benchmark_root/php-silex/puppet/php5-fpm.conf',
        require =>  Package['php5-fpm'],
        notify  =>  Service['php5-fpm']

    }
}

class composer {
    exec { 'apt-get install curl':
        command =>  '/usr/bin/apt-get -y install curl'
    }->
    exec { 'downloadcomposer':
        command =>  'curl -s http://getcomposer.org/installer | php',
        path    =>  ['/usr/bin', '/usr/sbin', '/bin']
    }->
    exec { 'move to bin':
        command => 'mv composer.phar /usr/local/bin/composer',
        path    =>  ['/usr/bin', '/usr/sbin', '/bin']
    }->
    exec { 'composer install':
        command => 'composer install',
        path    =>  ['/usr/bin', '/usr/sbin', '/bin', '/usr/local/bin'],
        cwd     =>  '/benchmark_root/php-silex/'
    }
}

include nginx, php5-fpm, composer

