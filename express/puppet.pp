
class nginx {
    file { '/etc/nginx/sites-enabled/express.conf':
        ensure  =>  present,
        source  =>  '/benchmark_root/express/puppet/nginx.conf',
        require =>  Package['nginx'],
        notify  =>  Service['nginx']
    }
}

class nodejs {
    exec { 'apt-get install -y python-software-properties python g++ make libmysqlclient-dev':
        command =>  '/usr/bin/apt-get install -y python-software-properties python g++ make'
    }->
    exec { 'add-apt-repository -y ppa:chris-lea/node.js':
        command => '/usr/bin/add-apt-repository -y ppa:chris-lea/node.js'
    }->
    exec { 'apt-get update after node':
        command => '/usr/bin/apt-get update'
    }->
    exec { 'apt-get install nodejs npm':
        command => '/usr/bin/apt-get install nodejs'
    }->
    exec { 'npm install --no-bin-links':
        command => '/usr/bin/npm install --no-bin-links',
        cwd     => '/benchmark_root/express/'
    }
}

class mongodb {
    exec { 'apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv 7F0CEB10':
        command => '/usr/bin/apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv 7F0CEB10'
    }->
    exec { "echo 'deb http://downloads-distro.mongodb.org/repo/ubuntu-upstart dist 10gen' | sudo tee /etc/apt/sources.list.d/mongodb.list":
        command => "/usr/bin/echo 'deb http://downloads-distro.mongodb.org/repo/ubuntu-upstart dist 10gen' | tee /etc/apt/sources.list.d/mongodb.list"
    }->
    exec { 'apt-get update after mongodb':
        command => '/usr/bin/apt-get update'
    }->
    exec { 'apt-get install mongodb-10gen':
        command => '/usr/bin/apt-get install mongodb-10gen'
    }
}


include nginx, nodejs, mongodb

