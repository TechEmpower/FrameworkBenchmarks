# Copyright © 2016, Microsoft Corporation. All rights reserved.
# :: ======================================================= ::

#====================================================================================
# Initialize
#====================================================================================
PARAM($uacConsent)

#====================================================================================
# Main
#====================================================================================

# Verifying root cause only if user choose to enable UAC with default settings.
if($uacConsent -eq $true)
{
	$detected = $true
	$uacAdminValue = Get-ItemProperty 'registry::HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\Windows\CurrentVersion\Policies\System' -Name 'ConsentPromptBehaviorAdmin' -ErrorAction SilentlyContinue
	$uacSecureDesktopValue = Get-ItemProperty 'registry::HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\Windows\CurrentVersion\Policies\System' -Name 'PromptOnSecureDesktop' -ErrorAction SilentlyContinue
	$uacEnableLUAValue = Get-ItemProperty 'registry::HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\Windows\CurrentVersion\Policies\System' -Name 'EnableLUA' -ErrorAction SilentlyContinue

	# Check for the ConsentPromptBehaviorAdmin value
	$detected = ($uacAdminValue.ConsentPromptBehaviorAdmin -eq 0) -or ($uacSecureDesktopValue.PromptOnSecureDesktop -eq 0)-or ($uacEnableLUAValue.EnableLUA -eq 0)

	Update-DiagRootCause -Id 'RC_UAC' -Detected $detected -Parameter @{'uacConsent' = $uacConsent}
}
