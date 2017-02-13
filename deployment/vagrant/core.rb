
def provision_bootstrap(config, role)

  # TODO this will break if the environment contains the ' delimiter, 
  # so at some point we need to escape the ' character here and unescape
  # it in bootstrap.sh
  config.vm.provision "shell" do |sh|
    sh.path = "bootstrap.sh"
    sh.privileged = false
  end
end

def provider_virtualbox(config, role, ip_address='172.16.0.16')
  config.vm.network "private_network", ip: ip_address
  
  config.vm.provider :virtualbox do |vb, override|
    override.vm.hostname = "TFB-#{role}"

    # Valid values are 32 and 64
    arch = ENV.fetch('TFB_VB_ARCH','64')

    # Value values are precise, trusty, etc
    code = ENV.fetch('TFB_VB_CODE','trusty')
    
    override.vm.box = "ubuntu/" + code + arch
    
    if ENV.fetch('TFB_SHOW_VM', false)
      vb.gui = true
    end

    # Improve Windows VirtualBox DNS resolution speed
    # Addresses mitchellh/vagrant#1807 and TechEmpower/FrameworkBenchmarks#1288
    vb.customize ["modifyvm", :id, "--natdnshostresolver1", "on"]
    vb.customize ["modifyvm", :id, "--natdnsproxy1", "on"]

    vb.memory = ENV.fetch('TFB_VB_MEM', 3022)
    vb.cpus = ENV.fetch('TFB_VB_CPU', 2)

    # The VirtualBox file system for shared folders (vboxfs)
    # does not support posix's chown/chmod - these can only 
    # be set at mount time, and they are uniform for the entire
    # shared directory. To mitigate the effects, we set the 
    # folders and files to 777 permissions. 
    # With 777 and owner vagrant *most* of the software works ok.
    # Occasional issues are still possible. 
    #
    # See mitchellh/vagrant#4997
    # See http://superuser.com/a/640028/136050
    override.vm.synced_folder "../..", "/home/vagrant/FrameworkBenchmarks"

    if role.eql? "all" or role.eql? "app"
      override.vm.network :forwarded_port, guest: 8080, host: 28080
    end
  end
end
