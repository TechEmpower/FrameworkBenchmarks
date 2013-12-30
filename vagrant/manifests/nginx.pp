class nginx {
    package { 'nginx':
        ensure  =>  present
    }
    file { '/etc/nginx/sites-enabled/default':
        ensure  =>  absent,
        require =>  Package['nginx'],
        notify  =>  Service['nginx']
    }

    service { 'nginx':
        ensure  =>  running,
        require =>  Package['nginx']
    }
}
include nginx
