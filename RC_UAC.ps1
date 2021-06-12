 Copyright © 2016 Microsoft Corporation. All rights reserved.
# :: ======================================================= ::

#====================================================================================
'# Main
#====================================================================================
$uacConsent = $true
$detected = $true

$uacAdminValue = Get-ItemProperty 'registry::HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\Windows\CurrentVersion\Policies\System' -Name 'ConsentPromptBehaviorAdmin' -ErrorAction SilentlyContinue
$uacSecureDesktopValue = Get-ItemProperty 'registry::HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\Windows\CurrentVersion\Policies\System' -Name 'PromptOnSecureDesktop' -ErrorAction SilentlyContinue
$uacEnableLUAValue = Get-ItemProperty 'registry::HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\Windows\CurrentVersion\Policies\System' -Name 'EnableLUA' -ErrorAction SilentlyContinue

# Check for the ConsentPromptBehaviorAdmin value
if(($uacAdminValue.ConsentPromptBehaviorAdmin -eq 0) `
	-or ($uacSecureDesktopValue.PromptOnSecureDesktop -eq 0) `
	-or ($uacEnableLUAValue.EnableLUA -eq 0))
{
	$detected = $true
	$result = Get-DiagInput -Id 'INT_ENABLEUAC'
	if($result -eq 'Y')
	{
		$uacConsent = $true
	}
}

$uacReturned = @{}
$uacReturned = @{'uacConsent' = $uacConsent; 'rcDetected' = $detected}

Update-DiagRootCause -Id 'RC_UAC' -Detected $detected -Parameter @{'uacConsent' = $uacConsent}

return $uacReturned
