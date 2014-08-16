#!/bin/bash
# 
# This script can be used to generate the required, machine-specific 
# configuration file:  GeminiHello-[MACHINE_NAME].conf
# 
# The intent is that developers would run this script once as part 
# of their initial setup.  On Windows, the script can be run through 
# Git bash.

projectName="GeminiHello"
machineNameUpper="`echo $HOSTNAME|tr '[a-z]' '[A-Z]'`"
machineNameLower="`echo $HOSTNAME|tr '[A-Z]' '[a-z]'`"
directoryName=$(cd `dirname $0` && pwd)
user=`echo \`id -nu\``

# If the file we wanted to generate already exists, don't bother 
# creating a new one.

configurationFile=$directoryName/Docroot/WEB-INF/$projectName-$machineNameUpper.conf;

if [ -f $configurationFile ]
then
  echo "The configuration file $configurationFile already exists.  Exiting."
  exit 0
else
  echo "Creating configuration file:  $configurationFile"
fi

# Write the required configuration attributes to the file.

echo "# -----------------------------------------------------------------------" > $configurationFile
echo "# GEMINIHELLO configuration file                                        " >> $configurationFile
echo "#                                                                        " >> $configurationFile
echo "# MACHINE-SPECIFIC CONFIGURATION                                         " >> $configurationFile
echo "#                                                                        " >> $configurationFile
echo "# The configuration attributes specified in this file are specific to a  " >> $configurationFile
echo "# single development-environment machine.                                " >> $configurationFile
echo "# -----------------------------------------------------------------------" >> $configurationFile
echo "" >> $configurationFile
echo "# Extend the development configuration, which in turn extends the " >> $configurationFile
echo "# baseline configuration." >> $configurationFile
echo "Extends = $projectName-Dev.conf" >> $configurationFile
echo "" >> $configurationFile
echo "# Now set any attributes that are specific to this machine." >> $configurationFile
echo "StandardDomain = http://$machineNameLower.techempower.com" >> $configurationFile
echo "SecureDomain = https://$machineNameLower.techempower.com" >> $configurationFile
echo "#OutboundMailEnabled = yes" >> $configurationFile
echo "StartupMailAuthor = $user@techempower.com" >> $configurationFile
echo "FromEmailAddress = $user@techempower.com" >> $configurationFile
echo "PasswordReset.FromAddress = $user@techempower.com" >> $configurationFile
echo "EmailExceptionHandler.ToEmailAddress = $user@techempower.com" >> $configurationFile
echo "EmailExceptionHandler.FromEmailAddress = $user@techempower.com" >> $configurationFile

exit 0
