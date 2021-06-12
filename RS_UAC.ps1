# Copyright © 2016, Microsoft Corporation. All rights reserved.
# :: ======================================================= ::

#====================================================================================
# Initialize
#====================================================================================
PARAM($uacConsent)

#====================================================================================
# Main
#====================================================================================
if($uacConsent -eq $true)
{
	$key = 'HKLM:\Software\Microsoft\Windows\CurrentVersion\Policies\System\'

	# Assign a value for ConsentPromptBehaviorAdmin key
	New-ItemProperty -Path $key -Name 'ConsentPromptBehaviorAdmin' -PropertyType DWord -Value '5' -Force
	
	# Assign a value for PromptOnSecureDesktop key
	New-ItemProperty -Path $key -Name 'PromptOnSecureDesktop' -PropertyType DWord -Value '1' -Force

	# Assign a value for EnableLUA key
	New-ItemProperty -Path $key -Name 'EnableLUA' -PropertyType DWord -Value '1' -Force
}
