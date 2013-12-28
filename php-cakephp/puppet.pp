
class nginx {
    package { 'nginx':
        ensure  =>  present
    }
    file { '/etc/nginx/sites-enabled/default':
        ensure  =>  absent,
        require =>  Package['nginx'],
        notify  =>  Service['nginx']
    }
    file { '/etc/nginx/sites-enabled/php-cakephp.conf':
        ensure  =>  present,
        source  =>  '/vagrant/php-cakephp/puppet/nginx.conf',
        require =>  Package['nginx'],
        notify  =>  Service['nginx']
    }
    service { 'nginx':
        ensure  =>  running,
        require =>  Package['nginx']
    }
}

class php5-fpm {
    require nginx
    package {'php5-fpm':
        ensure  =>  installed
    }
    package {'php5-cli':
        ensure  =>  installed
    }
    package { 'php-apc':
        ensure => installed
    }
    package { 'php5-mysql':
        ensure  =>  installed
    }
    file { '/etc/php5/fpm/pool.d/php-cakephp.conf':
        ensure  =>  present,
        source  =>  '/vagrant/php-cakephp/puppet/php5-fpm.conf',
        require =>  Package['php5-fpm'],
        notify  =>  Service['php5-fpm']

    }
    service { 'php5-fpm':
        ensure  =>  running,
        require =>  Package['php5-fpm']
    }
}


class mysql {
    package { 'mysql-client':
        ensure => present,
    }

    package { 'mysql-server':
        ensure => present,
    }

    service { 'mysql':
        ensure  =>  running,
        require =>  Package['mysql-server']
    }
}

include nginx, php5-fpm, mysql

