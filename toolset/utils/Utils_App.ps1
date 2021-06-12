# Copyright © 2016, Microsoft Corporation. All rights reserved.
# :: ======================================================= ::

<#
	DESCRIPTION:
	  Utils_Apps.ps1 is used as common scripts by Apps Troubleshooter 
	  
	FUNCTIONS:
	  Get-CurrentUser
	  Get-PendingReboot
#>

#====================================================================================
# Functions
#====================================================================================

Function Write-ExceptionTelemetry($FunctionName, [System.Management.Automation.ErrorRecord] $ex)
{
    <#
    DESCRIPTION:
      Writes script error information into telemetry stream
     
    EXAMPLE:
      try
      {
         0 / 0
      }
      catch [System.Exception]
      {
         Write-ExceptionTelemetry "DivideByZeroTest" $_
      }
    #>

    $ExceptionMessage = $ex.Exception.Message
    $ScriptFullPath = $ex.InvocationInfo.ScriptName
    $ExceptionScript = [System.IO.Path]::GetFileName($ScriptFullPath)
    $ExceptionScriptLine = $ex.InvocationInfo.ScriptLineNumber
    $ExceptionScriptColumn = $ex.InvocationInfo.OffsetInLine
    
	Write-DiagTelemetry "ScriptError" "$ExceptionScript\$FunctionName\$ExceptionScriptLine\$ExceptionScriptColumn\$ExceptionMessage"
}

function Get-CurrentUser
{
    <#
    DESCRIPTION:
      This function will list the information about currently logged in user. It will detect currently logged in user 
      via getting the owner of msdt.exe process.
     
    EXAMPLE:
      
      PS C:\> Get-CurrentUser
     
      Domain              : DESKTOP-747BNA3
      User                : Administrator
      SID                 : S-1-5-21-2146773085-903363285-719344707-1888833
      FullName            : Michael Glenn DESKTOP-747BNA3\Administrator
      LocalProfilePath    : C:\Users\Administrator
      LastUseTime         : 06/12/2021 
8:29 AM
      IsCurrentlyLoggedOn : True
      IsTemporaryProfile  : False

      This example will return an object which contains above properties respective to currently logged in or impersonated user.
    #>
    Description
    # Getting parent msdt.exe processes
    $msdtProcess = Get-WmiObject -Class Win32_Process -Filter "Name = 'msdt.exe'" | 
        Sort-Object -Property CreationDate -Descending | Select-Object -Last 1

    if($msdtProcess)
    {
        # Retrieving user information from msdt.exe processes
        $userDomain = ($msdtProcess.GetOwner()).Domain
        $userName = ($msdtProcess.GetOwner()).User
        $ntAccount = New-Object System.Security.Principal.NTAccount("$userDomain\$userName")
        $sid = $ntAccount.Translate([System.Security.Principal.SecurityIdentifier]).Value
        $win32UserProfiles = Get-WmiObject -Class Win32_UserProfile -Filter "SID = '$sid'"

        $currentUser = "" | Select Domain, User, SID, FullName, LocalProfilePath, LastUseTime, IsCurrentlyLoggedOn, IsTemporaryProfile
        foreach($win32UserProfile in $win32UserProfiles)
        {
            if($win32UserProfile.Loaded)
            {
                # Populating user profile
                $currentUser.Domain = $userDomain
                $currentUser.User = $userName
                $currentUser.SID = $sid
                $currentUser.FullName = ("$userDomain\$userName")
                $currentUser.LocalProfilePath = $win32UserProfile.LocalPath
                $currentUser.LastUseTime = ([WMI]'').ConvertToDateTime(($win32UserProfile.LastUseTime | Where {$_ -ne $null}))
                $currentUser.IsCurrentlyLoggedOn = $win32UserProfile.Loaded
                $currentUser.IsTemporaryProfile = ($win32UserProfile.Status -eq 1) # (0 = Default/Unset, 1 = Temporary, 2 = Roaming, 4 = Mandatory, 8 = Corrupted)
                
                break
            }
        }

        $currentUser
    }
}

function Get-PendingReboot
{
    <#
    SYNOPSIS:
        Gets the pending reboot status on a local computer.

    DESCRIPTION:
        This function will query the registry on a local or remote computer and determine if the
        system is pending a reboot, from Microsoft updates, Configuration Manager Client SDK, Pending Computer 
        Rename, Domain Join or Pending File Rename Operations. For Windows 2008+ the function will query the 
        CBS registry key as another factor in determining pending reboot state.  "PendingFileRenameOperations" 
        and "Auto Update\RebootRequired" are observed as being consistent across Windows Server 2003 & 2008.
	
        CBServicing			   : Component Based Servicing (Windows 2008+)
        WindowsUpdate		   : Windows Update / Auto Update (Windows 2003+)
        CCMClientSDK		   : SCCM 2012 Clients only (DetermineIfRebootPending method) otherwise $null value
        PendingComputerRename  : Detects either a computer rename or domain join operation (Windows 2003+)
		PendingFileRename	   : PendingFileRenameOperations (Windows 2003+)
		PendingFileRenameValue : PendingFileRenameOperations registry value; used to filter if needed, some Anti-Virus 
							    leverage this key for def/dat removal, giving a false positive PendingReboot

    EXAMPLE:
        PS C:\> Get-PendingReboot
	
        CBServicing            : False
        WindowsUpdate          : True
        CCMClient              : False
        PendingComputerRename  : False
        PendingFileRename      : False
        PendingFileRenameValue : {\??\C:\Users\Administrator\AppData\Local\Temp\nsz994B.tmp\p\syschk.dll, , \??\C:\Users\Administrator\AppData\Local\Temp\nsz994B.tmp\p\, ...}
        RebootPending          : True
	
        This example will query the local machine for pending reboot information.
	
    LINK:
        Component-Based Servicing:
        http://technet.microsoft.com/en-us/library/cc756291(v=WS.10).aspx
	
        PendingFileRename/Auto Update:
        http://support.microsoft.com/kb/2723674
        http://technet.microsoft.com/en-us/library/cc960241.aspx
        http://blogs.msdn.com/b/hansr/archive/2006/02/17/patchreboot.aspx

        SCCM 2012/CCM_ClientSDK:
        http://msdn.microsoft.com/en-us/library/jj902723.aspx
    #>

    try
    {
        $Computer = $env:COMPUTERNAME

        # Setting pending values to false
	    $CBSRebootPending = $PendingComputerRename = $PendingFileRename = $SCCM = $Pending = $false
    						
	    # Making registry connection to the computer
	    $HKLM = [UInt32] '0x80000002'
	    $WmiRegistryConnection = [WMIClass] "\\$Computer\root\default:StdRegProv"
						
	    # Querying the CBS Registry Key
	    $RegSubKeysCBS = $WmiRegistryConnection.EnumKey($HKLM,'SOFTWARE\Microsoft\Windows\CurrentVersion\Component Based Servicing\')
	    $CBSRebootPending = $RegSubKeysCBS.sNames -Contains 'RebootPending'		
					
	    # Query WUAU from the registry
	    $RegWUAURebootRequired = $WmiRegistryConnection.EnumKey($HKLM,'SOFTWARE\Microsoft\Windows\CurrentVersion\WindowsUpdate\Auto Update\')
	    $WUAURebootRequired = $RegWUAURebootRequired.sNames -contains 'RebootRequired'
						
	    # Query PendingFileRenameOperations from the registry
	    $RegSubKeySM = $WmiRegistryConnection.GetMultiStringValue($HKLM,'SYSTEM\CurrentControlSet\Control\Session Manager\","PendingFileRenameOperations')
	    $RegValuePFRO = $RegSubKeySM.sValue

	    # Query JoinDomain key from the registry - These keys are present if pending a reboot from a domain join operation
	    $Netlogon = $WmiRegistryConnection.EnumKey($HKLM,'SYSTEM\CurrentControlSet\Services\Netlogon').sNames
	    $PendingDomainJoin = ($Netlogon -contains 'JoinDomain') -or ($Netlogon -contains 'AvoidSpnSet')

	    # Query ComputerName and ActiveComputerName from the registry
	    $ActiveComputerName = $WmiRegistryConnection.GetStringValue($HKLM,'SYSTEM\CurrentControlSet\Control\ComputerName\ActiveComputerName\','ComputerName')            
	    $ComputerName = $WmiRegistryConnection.GetStringValue($HKLM,'SYSTEM\CurrentControlSet\Control\ComputerName\ComputerName\','ComputerName')

	    if (($ActiveComputerName -ne $ComputerName) -or $PendingDomainJoin) 
        { 
            $PendingComputerRename = $true 
        }
						
	    # If PendingFileRenameOperations has a value set $RegValuePFRO variable to $true
	    if ($RegValuePFRO) 
        { 
            $PendingFileRename = $true 
        }

	    # Determine SCCM 2012 Client Reboot Pending Status
	    # To avoid nested 'if' statements and unneeded WMI calls to determine if the CCM_ClientUtilities class exist, setting EA = 0
	    $CCMClientSDK = $null
	    $CCMSplat = @{
	        NameSpace = 'ROOT\ccm\ClientSDK'
	        Class = 'CCM_ClientUtilities'
	        Name = 'DetermineIfRebootPending'
	        ComputerName = $Computer
	        ErrorAction = 'Stop'
	    }

	    try 
        {
	        $CCMClientSDK = Invoke-WmiMethod @CCMSplat
	    } 
        catch [System.UnauthorizedAccessException] 
        {
	        $CcmStatus = Get-Service -Name CcmExec -ComputerName $Computer -ErrorAction SilentlyContinue
	        if ($CcmStatus.Status -ne 'Running') 
            {
	            Write-Warning "$Computer`: Error - CcmExec service is not running."
	            $CCMClientSDK = $null
	        }
			Write-ExceptionTelemetry "Get-PendingReboot" $_
	    } 
        catch 
        {
	        $CCMClientSDK = $null
			Write-ExceptionTelemetry "Get-PendingReboot" $_
	    }

	    if ($CCMClientSDK) 
        {
	        if ($CCMClientSDK.ReturnValue -ne 0) 
            {
		        Write-Warning "Error: DetermineIfRebootPending returned error code $($CCMClientSDK.ReturnValue)"          
		    }
		
            if ($CCMClientSDK.IsHardRebootPending -or $CCMClientSDK.RebootPending) 
            {
		        $SCCM = $true
		    }
	    }
	    else 
        {
	        $SCCM = $null
	    }

	    # Creating Custom PSObject and Select-Object Splat
	    $SelectSplat = @{
	        Property = (
	            'CBServicing',
	            'WindowsUpdate',
	            'CCMClientSDK',
	            'PendingComputerRename',
	            'PendingFileRename',
	            'PendingFileRenameValue',
	            'RebootPending'
	        )}
	    New-Object -TypeName PSObject -Property @{
	        CBServicing = $CBSRebootPending
	        WindowsUpdate = $WUAURebootRequired
	        CCMClientSDK = $SCCM
	        PendingComputerRename = $PendingComputerRename
	        PendingFileRename = $PendingFileRename
	        PendingFileRenameValue = $RegValuePFRO
	        RebootPending = ($PendingComputerRename -or $CBSRebootPending -or $WUAURebootRequired -or $SCCM -or $PendingFileRename)
	    } | Select-Object @SelectSplat

    } 
    catch 
    {
	    Write-Warning "$Computer`: $_"
	    # Updating diagnostics report
	    if ($ErrorLog) {
	        Out-File -InputObject "$Computer`,$_" -FilePath $ErrorLog -Append
	    }
		Write-ExceptionTelemetry "Get-PendingReboot" $_		
    }			
}
