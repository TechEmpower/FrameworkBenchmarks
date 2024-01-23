def provision_bootstrap(config)

  # TODO this will break if the environment contains the ' delimiter,
  # so at some point we need to escape the ' character here and unescape
  # it in bootstrap.sh
  config.vm.provision "shell" do |sh|
    sh.path = "bootstrap.sh"
    sh.privileged = false
  end
end

def provider_libvirt(config)
  config.vm.provider :libvirt do |virt, override|
    override.vm.hostname = "TFB-all"
    override.vm.box = "generic/ubuntu1804"

    unless ENV.fetch('TFB_SHOW_VM', false)
      virt.graphics_type = "none"
    end

    virt.memory = ENV.fetch('TFB_KVM_MEM', 3022)
    virt.cpus = ENV.fetch('TFB_KVM_CPU', 2)

    override.vm.synced_folder "../..", "/home/vagrant/FrameworkBenchmarks", type: "nfs", nfs_udp: false
  end
end

def provider_virtualbox(config)
  config.vm.provider :virtualbox do |vb, override|
    override.vm.hostname = "TFB-all"
    override.vm.box = "ubuntu/bionic64"

    # Allow increase in size for /dev/sda1
    # Would need plugin:
    #  vagrant plugin install vagrant-disksize
    if ENV.fetch('TFB_DISKSIZE', "0") != "0"
      override.disksize.size = ENV.fetch('TFB_DISKSIZE')
    end

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
  end
end
