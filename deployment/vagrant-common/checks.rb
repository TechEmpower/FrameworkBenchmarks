# Helps with finding current OS
module OS
  def OS.windows?
    (/cygwin|mswin|mingw|bccwin|wince|emx/ =~ RUBY_PLATFORM) != nil
  end

  def OS.mac?
   (/darwin/ =~ RUBY_PLATFORM) != nil
  end

  def OS.unix?
    !OS.windows?
  end

  def OS.linux?
    OS.unix? and not OS.mac?
  end
end

# Helps with finding current OS
module ARCH
  def ARCH.is64?
    (/x86_64/ =~ RUBY_PLATFORM) != nil
  end

  def ARCH.is32?
    !ARCH.is64?
  end
end

def get_provider()
  # Workaround for mitchellh/vagrant#1867
  if (ARGV[1] and ARGV[1].split('=')[0] == "--provider" and ARGV[1].split('=')[1])
    provider = ARGV[1].split('=')[1].to_sym
  else
    provider = (ARGV[2] || ENV['VAGRANT_DEFAULT_PROVIDER'] || :virtualbox).to_sym
  end
  provider
end

def check_provider_needs(provider)
  if provider == :"aws"
    check_aws_needs
  elsif provider == :"virtualbox"
    check_vb_needs
  end
end

def check_aws_needs()
  # Check required variables
  if !(ENV['TFB_AWS_ACCESS_KEY'] and ENV['TFB_AWS_SECRET_KEY'] \
        and ENV['TFB_AWS_KEY_NAME'] and ENV['TFB_AWS_KEY_PATH'])
    abort 'If you want to use the AWS provider, you must provide these four variables: 
    TFB_AWS_ACCESS_KEY : Your Amazon Web Services Access Key
    TFB_AWS_SECRET_KEY : Your Amazon Web Services Secret Access Key
    TFB_AWS_KEY_NAME   : The name of the keypair you are using
    TFB_AWS_KEY_PATH   : Path to the *.pem file for the keypair you are using'
  end

  # Print warning
    warning = "\033[33m\
WARNING: FrameworkBenchmarks is disabling folder sync between your
local working copy and Amazon Web Services - the ~/FrameworkBenchmarks
directory in your VM will be a git clone of TechEmpower/FrameworkBenchmarks. 
You can re-enable folder sync using 
    $ TFB_FORCE_SYNC=true vagrant up --provider=aws
but be aware that you will need to upload ~2GB to Amazon before you can use
the VM as normal.\033[0m"
  puts warning
end

def check_vb_needs()
  # Check if this computer can run a 64-bit OS
  warning = "\033[31m\
WARNING: FrameworkBenchmarks only officially supports a 64-bit 
virtual machine, which your current system may not be able to 
support. Use `TFB_SHOW_VM=true vagrant up` to watch the VM launch -
if you just see a black window you likely cannot run a 64-bit VM. 

To workaround, consider using the Amazon (e.g. AWS) provider
   $ vagrant up --provider=aws

Or forcing FrameworkBenchmarks to attempt a 32-bit VM
   $ TFB_VB_ARCH=32 vagrant up
  
  See http://askubuntu.com/questions/41550 for more info\033[0m"

  # AMD-based needs svm feature, Intel-based needs vmx feature
  if OS.linux? and %x(egrep '(vmx|svm)' /proc/cpuinfo).empty?
    puts warning
  end

  # Ignore PowerPC, check for intel features
  if OS.mac? and %x(sysctl -n machdep.cpu.features | grep -i vmx).empty?
    puts warning
  end

  # Don't really know how to check CPU features, so I'll just check 
  # the arch
  if OS.windows? and ARCH.is32?
    puts warning
  end
end

