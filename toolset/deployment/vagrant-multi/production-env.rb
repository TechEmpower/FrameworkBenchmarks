
def provision_bootstrap(config, role)
  config.vm.provision "shell" do |sh|
    sh.path = "bootstrap.sh"
    sh.privileged = false
    sh.args = role
  end
end

def provider_aws(config, role, ip_address)
  config.vm.provider :aws do |aws, override|
    aws.access_key_id = ENV['TFB_AWS_ACCESS_KEY'] 
    aws.secret_access_key = ENV['TFB_AWS_SECRET_KEY']
    aws.keypair_name = ENV['TFB_AWS_KEY_NAME']
    override.ssh.private_key_path = ENV['TFB_AWS_KEY_PATH']

    override.vm.box = "dummy"
    override.vm.box_url = "https://github.com/mitchellh/vagrant-aws/raw/master/dummy.box"
    aws.ami = "ami-62c8160a"
    override.ssh.username = "ubuntu"
    
    aws.private_ip_address = ip_address
    aws.associate_public_ip = true
    aws.subnet_id = "subnet-2737230f"
    aws.security_groups = ["sg-871240e2"]

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
    override.vm.network "private_network", ip: ip_address
    
    override.vm.synced_folder "../../..", "/FrameworkBenchmarks"
  end
end