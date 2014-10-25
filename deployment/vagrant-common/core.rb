
def provision_bootstrap(config, role)
  # Find all environment variables that begin with 
  # TFB_* and pass them as an argument. This is a hack to 
  # let our bootstrap script use environment variables that 
  # were originally defined on the host 
  # Skip any potentially sensitive variables
  env_arg = ""
  skip= ['TFB_AWS_ACCESS_KEY', 'TFB_AWS_SECRET_KEY', 'TFB_AWS_KEY_NAME', 'TFB_AWS_KEY_PATH']
  ENV.each do |key, array|
    if (key.start_with?("TFB") && !skip.include?(key))
      env_arg.concat key
      env_arg.concat "="
      env_arg.concat array
      env_arg.concat "\n"
    end
  end
  env_arg = env_arg.strip

  # TODO this will break if the environment contains the ' delimiter, 
  # so at some point we need to escape the ' character here and unescape
  # it in bootstrap.sh
  config.vm.provision "shell" do |sh|
    sh.path = "../vagrant-common/bootstrap.sh"
    sh.privileged = false
    sh.args = "'#{env_arg}' '#{role}'"
  end
end

def provider_aws(config, role, ip_address='172.16.0.16')
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
    aws.ami = "ami-f6bf659e"
    override.ssh.username = "ubuntu"
    
    aws.private_ip_address = ip_address
    aws.associate_public_ip = true
    aws.region = ENV.fetch('TFB_AWS_REGION', 'us-east-1')
    aws.subnet_id = ENV['TFB_AWS_SUBNET']  # subnet-2737230f for me
    aws.security_groups = [ENV['TFB_AWS_SEC_GROUP']] # sg-871240e2 
    aws.instance_type = ENV.fetch('TFB_AWS_EC2_TYPE', 'm1.large')

    aws.tags = {
      'Project' => 'FrameworkBenchmarks',
      'TFB_role' => role
     }
    
    # Setup disk. Defauly is 15GB General Purpose SSD
    # Double the default volume size, as we download and 
    # install a *lot* of stuff
    # Documentation is at http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-ec2-blockdev-template.html
    aws.block_device_mapping = [{ 'DeviceName' => '/dev/sda1', 
      'Ebs.VolumeSize' => 15 ,
      'Ebs.DeleteOnTermination' => ENV.fetch('TFB_AWS_EBS_DELETE', true),
      'Ebs.VolumeType' => ENV.fetch('TFB_AWS_EBS_TYPE', 'gp2')
      }]
    if ENV.fetch('TFB_AWS_EBS_TYPE', 'standard') == 'io1'
      aws.block_device_mapping[0]['Ebs.Iops'] = ENV.fetch('TFB_AWS_EBS_IO', '1000')
    end

    if ENV.fetch('TFB_FORCE_SYNC', "false") == "true"
      override.vm.synced_folder "../..", "/FrameworkBenchmarks"
    end

  end
end

def provider_virtualbox(config, role)
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

    vb.memory = ENV.fetch('TFB_VB_MEM', 2048)
    vb.cpus = ENV.fetch('TFB_VB_CPU', 2)

    override.vm.synced_folder "../..", "/FrameworkBenchmarks"

    if role.eql? "all" or role.eql? "app"
      override.vm.network :forwarded_port, guest: 8080, host: 28080
    end
  end
end