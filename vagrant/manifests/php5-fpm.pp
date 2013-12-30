
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
    service { 'php5-fpm':
        ensure  =>  running,
        require =>  Package['php5-fpm']
    }
}
include php5-fpm
