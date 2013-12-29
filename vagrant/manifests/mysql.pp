class mysql {
    package { 'mysql-client':
        ensure => present,
    }

    package { 'mysql-server':
        ensure => present,
    }
    exec { 'mysql --user root < /benchmark_root/config/create.sql >> /benchmark_root/vagrant/puppet-mysql.sql.log 2>&1':
        path    =>  ['/usr/bin', '/usr/sbin', '/bin'],
        timeout => 600,
        require =>  Package['mysql-server']
    }

    service { 'mysql':
        ensure  =>  running,
        require =>  Package['mysql-server']
    }
}
include mysql