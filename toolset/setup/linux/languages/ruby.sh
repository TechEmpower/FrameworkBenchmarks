. ../toolset/setup/linux/bash_functions.sh

curl -L get.rvm.io | bash -s head --auto-dotfiles
echo rvm_auto_reload_flag=2 >> ~/.rvmrc

fw_exists ~/.rvm/rubies/ruby-2.0.0-p0/
[ $? -ne 0 ] || { echo "Ruby is installed!"; return 0; }

. ~/.rvm/scripts/rvm
. ~/.profile
rvm install 2.0.0-p0
rvm 2.0.0-p0 do gem install bundler

# Install JRuby 
rvm install jruby-1.7.8
rvm jruby-1.7.8 do gem install bundler
