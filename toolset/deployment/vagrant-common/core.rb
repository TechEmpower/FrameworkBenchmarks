
def provision_bootstrap(config, role)
  # Find all environment variables that begin with 
  # TFB_* and pass them as an argument. This is a hack to 
  # let our bootstrap script use environment variables that 
  # were originally defined on the host 
  env_arg = ""
  ENV.each do |key, array|
    if key.start_with? "TFB"
      env_arg.concat key
      env_arg.concat "="
      env_arg.concat array
      env_arg.concat "\n"
    end
  end
  env_arg = env_arg.strip

  # TODO this will break if the environment contains the ' character, 
  # so at some point we need to escape the ' character here and unescape
  # it in bootstrap.sh
  config.vm.provision "shell" do |sh|
    sh.path = "../vagrant-common/bootstrap.sh"
    sh.privileged = false
    sh.args = "'#{env_arg}' '#{role}'"
  end
end

def provider_aws(config, role, ip_address)
  config.vm.provider :aws do |aws, override|
    aws.access_key_id = ENV['TFB_AWS_ACCESS_KEY'] 
    aws.secret_access_key = ENV['TFB_AWS_SECRET_KEY']
    aws.keypair_name = ENV['TFB_AWS_KEY_NAME']
    override.ssh.private_key_path = ENV['TFB_AWS_KEY_PATH']

    # Use a boilerplate box - this will be replaced by the AMI
    override.vm.box = "dummy"
    override.vm.box_url = "https://github.com/mitchellh/vagrant-aws/raw/master/dummy.box"
    
    # From http://cloud-images.ubuntu.com/trusty/current/
    # This is 64-bit Ubuntu 14.04 US east EBS
    # See http://cloud-images.ubuntu.com/vagrant/trusty/current/ 
    # for comparison to the Ubuntu Vagrant VirtualBox boxes 
    aws.ami = "ami-62c8160a"
    override.ssh.username = "ubuntu"


    aws.region = ENV.fetch('TFB_AWS_REGION', 'us-east-1')
    
    aws.private_ip_address = ip_address
    aws.associate_public_ip = true
    aws.subnet_id = ENV['TFB_AWS_SUBNET']  # subnet-2737230f for me
    aws.security_groups = [ENV['TFB_AWS_SEC_GROUP']] # sg-871240e2 

    aws.tags = {
      'Project' => 'FrameworkBenchmarks',
      'TFB_role' => role
     }
    
    # Double the default volume size, as we download and 
    # install a *lot* of stuff

    # TODO use http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-ec2-blockdev-template.html
    # and read the type from the environment
    aws.block_device_mapping = [{ 'DeviceName' => '/dev/sda1', 'Ebs.VolumeSize' => 15 }]

    aws.instance_type = "m1.small"
  end
end

def provider_virtualbox(config, role, ip_address)
  config.vm.provider :virtualbox do |vb, override|
    override.vm.hostname = "TFB-#{role}"

    override.vm.box = "ubuntu/trusty64"
    if ENV.fetch('TFB_VM_ARCH','64') == "32"
      override.vm.box = "ubuntu/trusty32"
    end

    # Use a non-standard value here as most home networks are 
    # 192.168.X.X or 10.X.X.X and we cannot have a collision 
    # with the host network
    override.vm.network "private_network", ip: ip_address
    
    if ENV.fetch('TFB_SHOW_VM', false)
      vb.gui = true
    end

    if ENV.fetch('TFB_VB_MEM', 2048)
      vb.customize ["modifyvm", :id, "--memory", "2048"]
    end

    override.vm.synced_folder "../../..", "/FrameworkBenchmarks"

    if role.eql? "all" or role.eql? "app"
      override.vm.network :forwarded_port, guest: 8080, host: 28080
    end
  end
end