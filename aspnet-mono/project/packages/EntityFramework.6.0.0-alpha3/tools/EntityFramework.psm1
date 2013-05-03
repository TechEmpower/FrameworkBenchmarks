# Copyright (c) Microsoft Corporation.  All rights reserved.

$InitialDatabase = '0'

$knownExceptions = @(
    'System.Data.Entity.Migrations.Infrastructure.MigrationsException',
    'System.Data.Entity.Migrations.Infrastructure.AutomaticMigrationsDisabledException',
    'System.Data.Entity.Migrations.Infrastructure.AutomaticDataLossException',
    'System.Data.Entity.Migrations.Infrastructure.MigrationsPendingException',
    'System.Data.Entity.Migrations.ProjectTypeNotSupportedException'
)

<#
.SYNOPSIS
    Enables Code First Migrations in a project.

.DESCRIPTION
    Enables Migrations by scaffolding a migrations configuration class in the project. If the
    target database was created by an initializer, an initial migration will be created (unless
    automatic migrations are enabled via the EnableAutomaticMigrations parameter).

.PARAMETER ContextTypeName
    Specifies the context to use. If omitted, migrations will attempt to locate a
    single context type in the target project.

.PARAMETER EnableAutomaticMigrations
    Specifies whether automatic migrations will be enabled in the scaffolded migrations configuration.
    If omitted, automatic migrations will be disabled.

.PARAMETER MigrationsDirectory
    Specifies the name of the directory that will contain migrations code files.
    If omitted, the directory will be named "Migrations". 

.PARAMETER ProjectName
    Specifies the project that the scaffolded migrations configuration class will
    be added to. If omitted, the default project selected in package manager
    console is used.

.PARAMETER StartUpProjectName
    Specifies the configuration file to use for named connection strings. If
    omitted, the specified project's configuration file is used.

.PARAMETER ConnectionStringName
    Specifies the name of a connection string to use from the application's
    configuration file.

.PARAMETER ConnectionString
    Specifies the the connection string to use. If omitted, the context's
    default connection will be used.

.PARAMETER ConnectionProviderName
    Specifies the provider invariant name of the connection string.

.PARAMETER Force
    Specifies that the migrations configuration be overwritten when running more
    than once for a given project.
#>
function Enable-Migrations
{
    [CmdletBinding(DefaultParameterSetName = 'ConnectionStringName')] 
    param (
        [string] $ContextTypeName,
        [alias('Auto')]
        [switch] $EnableAutomaticMigrations,
        [string] $MigrationsDirectory,
        [string] $ProjectName,
        [string] $StartUpProjectName,
        [parameter(ParameterSetName = 'ConnectionStringName')]
        [string] $ConnectionStringName,
        [parameter(ParameterSetName = 'ConnectionStringAndProviderName',
            Mandatory = $true)]
        [string] $ConnectionString,
        [parameter(ParameterSetName = 'ConnectionStringAndProviderName',
            Mandatory = $true)]
        [string] $ConnectionProviderName,
        [switch] $Force
    )

    $runner = New-MigrationsRunner $ProjectName $StartUpProjectName $null $ConnectionStringName $ConnectionString $ConnectionProviderName

    try
    {
        Invoke-RunnerCommand $runner System.Data.Entity.Migrations.EnableMigrationsCommand @( $EnableAutomaticMigrations.IsPresent, $Force.IsPresent ) @{ 'ContextTypeName' = $ContextTypeName; 'MigrationsDirectory' = $MigrationsDirectory }        		
		$error = Get-RunnerError $runner					

        if ($error)
        {
            if ($knownExceptions -notcontains $error.TypeName)
            {
                Write-Host $error.StackTrace
            }

            throw $error.Message
        }

		$(Get-VSComponentModel).GetService([NuGetConsole.IPowerConsoleWindow]).Show()	        
    }
    finally
    {				
        Remove-Runner $runner		
    }
}

<#
.SYNOPSIS
    Scaffolds a migration script for any pending model changes.

.DESCRIPTION
    Scaffolds a new migration script and adds it to the project.

.PARAMETER Name
    Specifies the name of the custom script.

.PARAMETER Force
    Specifies that the migration user code be overwritten when re-scaffolding an
    existing migration.

.PARAMETER ProjectName
    Specifies the project that contains the migration configuration type to be
    used. If omitted, the default project selected in package manager console
    is used.

.PARAMETER StartUpProjectName
    Specifies the configuration file to use for named connection strings. If
    omitted, the specified project's configuration file is used.

.PARAMETER ConfigurationTypeName
    Specifies the migrations configuration to use. If omitted, migrations will
    attempt to locate a single migrations configuration type in the target
    project.

.PARAMETER ConnectionStringName
    Specifies the name of a connection string to use from the application's
    configuration file.

.PARAMETER ConnectionString
    Specifies the the connection string to use. If omitted, the context's
    default connection will be used.

.PARAMETER ConnectionProviderName
    Specifies the provider invariant name of the connection string.

.PARAMETER IgnoreChanges
    Scaffolds an empty migration ignoring any pending changes detected in the current model.
    This can be used to create an initial, empty migration to enable Migrations for an existing
    database. N.B. Doing this assumes that the target database schema is compatible with the
    current model.

#>
function Add-Migration
{
    [CmdletBinding(DefaultParameterSetName = 'ConnectionStringName')]
    param (
        [parameter(Position = 0,
            Mandatory = $true)]
        [string] $Name,
        [switch] $Force,
        [string] $ProjectName,
        [string] $StartUpProjectName,
        [string] $ConfigurationTypeName,
        [parameter(ParameterSetName = 'ConnectionStringName')]
        [string] $ConnectionStringName,
        [parameter(ParameterSetName = 'ConnectionStringAndProviderName',
            Mandatory = $true)]
        [string] $ConnectionString,
        [parameter(ParameterSetName = 'ConnectionStringAndProviderName',
            Mandatory = $true)]
        [string] $ConnectionProviderName,
        [switch] $IgnoreChanges)

    $runner = New-MigrationsRunner $ProjectName $StartUpProjectName $ConfigurationTypeName $ConnectionStringName $ConnectionString $ConnectionProviderName

    try
    {
        Invoke-RunnerCommand $runner System.Data.Entity.Migrations.AddMigrationCommand @( $Name, $Force.IsPresent, $IgnoreChanges.IsPresent )
        $error = Get-RunnerError $runner       		

        if ($error)
        {
            if ($knownExceptions -notcontains $error.TypeName)
            {
                Write-Host $error.StackTrace
            }

            throw $error.Message
        }		
		$(Get-VSComponentModel).GetService([NuGetConsole.IPowerConsoleWindow]).Show()	        
    }
    finally
    {			
        Remove-Runner $runner		
    }	
}

<#
.SYNOPSIS
    Applies any pending migrations to the database.

.DESCRIPTION
    Updates the database to the current model by applying pending migrations.

.PARAMETER SourceMigration
    Only valid with -Script. Specifies the name of a particular migration to use
    as the update's starting point. If omitted, the last applied migration in
    the database will be used.

.PARAMETER TargetMigration
    Specifies the name of a particular migration to update the database to. If
    omitted, the current model will be used.

.PARAMETER Script
    Generate a SQL script rather than executing the pending changes directly.

.PARAMETER Force
    Specifies that data loss is acceptable during automatic migration of the
    database.

.PARAMETER ProjectName
    Specifies the project that contains the migration configuration type to be
    used. If omitted, the default project selected in package manager console
    is used.

.PARAMETER StartUpProjectName
    Specifies the configuration file to use for named connection strings. If
    omitted, the specified project's configuration file is used.

.PARAMETER ConfigurationTypeName
    Specifies the migrations configuration to use. If omitted, migrations will
    attempt to locate a single migrations configuration type in the target
    project.

.PARAMETER ConnectionStringName
    Specifies the name of a connection string to use from the application's
    configuration file.

.PARAMETER ConnectionString
    Specifies the the connection string to use. If omitted, the context's
    default connection will be used.

.PARAMETER ConnectionProviderName
    Specifies the provider invariant name of the connection string.
#>
function Update-Database
{
    [CmdletBinding(DefaultParameterSetName = 'ConnectionStringName')]
    param (
        [string] $SourceMigration,
        [string] $TargetMigration,
        [switch] $Script,
        [switch] $Force,
        [string] $ProjectName,
        [string] $StartUpProjectName,
        [string] $ConfigurationTypeName,
        [parameter(ParameterSetName = 'ConnectionStringName')]
        [string] $ConnectionStringName,
        [parameter(ParameterSetName = 'ConnectionStringAndProviderName',
            Mandatory = $true)]
        [string] $ConnectionString,
        [parameter(ParameterSetName = 'ConnectionStringAndProviderName',
            Mandatory = $true)]
        [string] $ConnectionProviderName)

    $runner = New-MigrationsRunner $ProjectName $StartUpProjectName $ConfigurationTypeName $ConnectionStringName $ConnectionString $ConnectionProviderName

    try
    {
        Invoke-RunnerCommand $runner System.Data.Entity.Migrations.UpdateDatabaseCommand @( $SourceMigration, $TargetMigration, $Script.IsPresent, $Force.IsPresent, $Verbose.IsPresent )
        $error = Get-RunnerError $runner
        
        if ($error)
        {
            if ($knownExceptions -notcontains $error.TypeName)
            {
                Write-Host $error.StackTrace
            }

            throw $error.Message
        }		
		$(Get-VSComponentModel).GetService([NuGetConsole.IPowerConsoleWindow]).Show()	        
    }
    finally
    {		     
	    Remove-Runner $runner
    }
}

<#
.SYNOPSIS
    Displays the migrations that have been applied to the target database.

.DESCRIPTION
    Displays the migrations that have been applied to the target database.

.PARAMETER ProjectName
    Specifies the project that contains the migration configuration type to be
    used. If omitted, the default project selected in package manager console
    is used.

.PARAMETER StartUpProjectName
    Specifies the configuration file to use for named connection strings. If
    omitted, the specified project's configuration file is used.

.PARAMETER ConfigurationTypeName
    Specifies the migrations configuration to use. If omitted, migrations will
    attempt to locate a single migrations configuration type in the target
    project.

.PARAMETER ConnectionStringName
    Specifies the name of a connection string to use from the application's
    configuration file.

.PARAMETER ConnectionString
    Specifies the the connection string to use. If omitted, the context's
    default connection will be used.

.PARAMETER ConnectionProviderName
    Specifies the provider invariant name of the connection string.
#>
function Get-Migrations
{
    [CmdletBinding(DefaultParameterSetName = 'ConnectionStringName')]
    param (
        [string] $ProjectName,
        [string] $StartUpProjectName,
        [string] $ConfigurationTypeName,
        [parameter(ParameterSetName = 'ConnectionStringName')]
        [string] $ConnectionStringName,
        [parameter(ParameterSetName = 'ConnectionStringAndProviderName',
            Mandatory = $true)]
        [string] $ConnectionString,
        [parameter(ParameterSetName = 'ConnectionStringAndProviderName',
            Mandatory = $true)]
        [string] $ConnectionProviderName)

    $runner = New-MigrationsRunner $ProjectName $StartUpProjectName $ConfigurationTypeName $ConnectionStringName $ConnectionString $ConnectionProviderName

    try
    {
        Invoke-RunnerCommand $runner System.Data.Entity.Migrations.GetMigrationsCommand
        $error = Get-RunnerError $runner
        
        if ($error)
        {
            if ($knownExceptions -notcontains $error.TypeName)
            {
                Write-Host $error.StackTrace
            }

            throw $error.Message
        }
    }
    finally
    {
        Remove-Runner $runner
    }
}

function New-MigrationsRunner($ProjectName, $StartUpProjectName, $ConfigurationTypeName, $ConnectionStringName, $ConnectionString, $ConnectionProviderName)
{
    $startUpProject = Get-MigrationsStartUpProject $StartUpProjectName $ProjectName
    Build-Project $startUpProject

    $project = Get-MigrationsProject $ProjectName
    Build-Project $project

    $installPath = Get-EntityFrameworkInstallPath $project
    $toolsPath = Join-Path $installPath tools

    $info = New-Object System.AppDomainSetup -Property @{
            ShadowCopyFiles = 'true';
            ApplicationBase = $installPath;
            PrivateBinPath = 'tools';
            ConfigurationFile = ([AppDomain]::CurrentDomain.SetupInformation.ConfigurationFile)
        }
    
    $targetFrameworkVersion = (New-Object System.Runtime.Versioning.FrameworkName ($project.Properties.Item('TargetFrameworkMoniker').Value)).Version

    if ($targetFrameworkVersion -lt (New-Object Version @( 4, 5 )))
    {
        $info.PrivateBinPath += ';lib\net40'
    }
    else
    {
        $info.PrivateBinPath += ';lib\net45'
    }

    $domain = [AppDomain]::CreateDomain('Migrations', $null, $info)
    $domain.SetData('project', $project)
    $domain.SetData('startUpProject', $startUpProject)
    $domain.SetData('configurationTypeName', $ConfigurationTypeName)
    $domain.SetData('connectionStringName', $ConnectionStringName)
    $domain.SetData('connectionString', $ConnectionString)
    $domain.SetData('connectionProviderName', $ConnectionProviderName)
    
    [AppDomain]::CurrentDomain.SetShadowCopyFiles()
    $utilityAssembly = [System.Reflection.Assembly]::LoadFrom((Join-Path $toolsPath EntityFramework.PowerShell.Utility.dll))
    $dispatcher = $utilityAssembly.CreateInstance(
        'System.Data.Entity.Migrations.Utilities.DomainDispatcher',
        $false,
        [System.Reflection.BindingFlags]::Instance -bor [System.Reflection.BindingFlags]::Public,
        $null,
        $PSCmdlet,
        $null,
        $null)        
    $domain.SetData('efDispatcher', $dispatcher)

    return @{
        Domain = $domain;
        ToolsPath = $toolsPath
    }
}

function Remove-Runner($runner)
{
    [AppDomain]::Unload($runner.Domain)
}

function Invoke-RunnerCommand($runner, $command, $parameters, $anonymousArguments)
{
    $domain = $runner.Domain

	if ($anonymousArguments)
	{
		$anonymousArguments.GetEnumerator() | %{
			$domain.SetData($_.Name, $_.Value)
		}
	}

    $domain.CreateInstanceFrom(
        (Join-Path $runner.ToolsPath EntityFramework.PowerShell.dll),
        $command,
        $false,
        0,
        $null,
        $parameters,
        $null,
        $null) | Out-Null
}

function Get-RunnerError($runner)
{
    $domain = $runner.Domain

    if (!$domain.GetData('wasError'))
    {
        return $null
    }

    return @{
            Message = $domain.GetData('error.Message');
            TypeName = $domain.GetData('error.TypeName');
            StackTrace = $domain.GetData('error.StackTrace')
    }
}

function Get-MigrationsProject($name, $hideMessage)
{
    if ($name)
    {
        return Get-SingleProject $name
    }

    $project = Get-Project
    $projectName = $project.Name

    if (!$hideMessage)
    {
        Write-Verbose "Using NuGet project '$projectName'."
    }

    return $project
}

function Get-MigrationsStartUpProject($name, $fallbackName)
{    
    $startUpProject = $null

    if ($name)
    {
        $startUpProject = Get-SingleProject $name
    }
    else
    {
        $startupProjectPaths = $DTE.Solution.SolutionBuild.StartupProjects

        if ($startupProjectPaths)
        {
            if ($startupProjectPaths.Length -eq 1)
            {
                $startupProjectPath = $startupProjectPaths[0]

                if (!(Split-Path -IsAbsolute $startupProjectPath))
                {
                    $solutionPath = Split-Path $DTE.Solution.Properties.Item('Path').Value
                    $startupProjectPath = Join-Path $solutionPath $startupProjectPath -Resolve
                }

                $startupProject = Get-SolutionProjects | ?{
					try
					{
						$fullName = $_.FullName
					}
					catch [NotImplementedException]
					{
						return $false
					}

                    if ($fullName -and $fullName.EndsWith('\'))
                    {
                        $fullName = $fullName.Substring(0, $fullName.Length - 1)
                    }

                    return $fullName -eq $startupProjectPath
                }
            }
            else
            {
                Write-Verbose 'More than one start-up project found.'
            }
        }
        else
        {
            Write-Verbose 'No start-up project found.'
        }
    }

    if (!($startUpProject -and (Test-StartUpProject $startUpProject)))
    {
        $startUpProject = Get-MigrationsProject $fallbackName $true
        $startUpProjectName = $startUpProject.Name

        Write-Warning "Cannot determine a valid start-up project. Using project '$startUpProjectName' instead. Your configuration file and working directory may not be set as expected. Use the -StartUpProjectName parameter to set one explicitly. Use the -Verbose switch for more information."
    }
    else
    {
        $startUpProjectName = $startUpProject.Name

        Write-Verbose "Using StartUp project '$startUpProjectName'."
    }

    return $startUpProject
}

function Get-SolutionProjects()
{
    $projects = New-Object System.Collections.Stack
    
    $DTE.Solution.Projects | %{
        $projects.Push($_)
    }
    
    while ($projects.Count -ne 0)
    {
        $project = $projects.Pop();
        
        # NOTE: This line is similar to doing a "yield return" in C#
        $project

        if ($project.ProjectItems)
        {
            $project.ProjectItems | ?{ $_.SubProject } | %{
                $projects.Push($_.SubProject)
            }
        }
    }
}

function Get-SingleProject($name)
{
    $project = Get-Project $name

    if ($project -is [array])
    {
        throw "More than one project '$name' was found. Specify the full name of the one to use."
    }

    return $project
}

function Test-StartUpProject($project)
{    
    if ($project.Kind -eq '{cc5fd16d-436d-48ad-a40c-5a424c6e3e79}')
    {
        $projectName = $project.Name
        Write-Verbose "Cannot use start-up project '$projectName'. The Windows Azure Project type isn't supported."
        
        return $false
    }
    
    return $true
}

function Build-Project($project)
{
    $configuration = $DTE.Solution.SolutionBuild.ActiveConfiguration.Name

    $DTE.Solution.SolutionBuild.BuildProject($configuration, $project.UniqueName, $true)

    if ($DTE.Solution.SolutionBuild.LastBuildInfo)
    {
        $projectName = $project.Name

        throw "The project '$projectName' failed to build."
    }
}

function Get-EntityFrameworkInstallPath($project)
{
    $package = Get-Package -ProjectName $project.FullName | ?{ $_.Id -eq 'EntityFramework' }

    if (!$package)
    {
        $projectName = $project.Name

        throw "The EntityFramework package is not installed on project '$projectName'."
    }
    
    return Get-PackageInstallPath $package
}
    
function Get-PackageInstallPath($package)
    {
    $componentModel = Get-VsComponentModel
    $packageInstallerServices = $componentModel.GetService([NuGet.VisualStudio.IVsPackageInstallerServices])

    $vsPackage = $packageInstallerServices.GetInstalledPackages() | ?{ $_.Id -eq $package.Id -and $_.Version -eq $package.Version }
    
    return $vsPackage.InstallPath
}

Export-ModuleMember @( 'Enable-Migrations', 'Add-Migration', 'Update-Database', 'Get-Migrations' ) -Variable InitialDatabase

# SIG # Begin signature block
# MIIanQYJKoZIhvcNAQcCoIIajjCCGooCAQExCzAJBgUrDgMCGgUAMGkGCisGAQQB
# gjcCAQSgWzBZMDQGCisGAQQBgjcCAR4wJgIDAQAABBAfzDtgWUsITrck0sYpfvNR
# AgEAAgEAAgEAAgEAAgEAMCEwCQYFKw4DAhoFAAQUhBFz4Itt1wDYyur2mJt4qMN2
# aESgghV5MIIEujCCA6KgAwIBAgIKYQKSSgAAAAAAIDANBgkqhkiG9w0BAQUFADB3
# MQswCQYDVQQGEwJVUzETMBEGA1UECBMKV2FzaGluZ3RvbjEQMA4GA1UEBxMHUmVk
# bW9uZDEeMBwGA1UEChMVTWljcm9zb2Z0IENvcnBvcmF0aW9uMSEwHwYDVQQDExhN
# aWNyb3NvZnQgVGltZS1TdGFtcCBQQ0EwHhcNMTIwMTA5MjIyNTU5WhcNMTMwNDA5
# MjIyNTU5WjCBszELMAkGA1UEBhMCVVMxEzARBgNVBAgTCldhc2hpbmd0b24xEDAO
# BgNVBAcTB1JlZG1vbmQxHjAcBgNVBAoTFU1pY3Jvc29mdCBDb3Jwb3JhdGlvbjEN
# MAsGA1UECxMETU9QUjEnMCUGA1UECxMebkNpcGhlciBEU0UgRVNOOkI4RUMtMzBB
# NC03MTQ0MSUwIwYDVQQDExxNaWNyb3NvZnQgVGltZS1TdGFtcCBTZXJ2aWNlMIIB
# IjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAzWPD96K1R9n5OZRTrGuPpnk4
# IfTRbj0VOBbBcyyZj/vgPFvhokyLsquLtPJKx7mTUNEm9YdTsHp180cPFytnLGTr
# YOdKjOCLXsRWaTc6KgRdFwHIv6m308mro5GogeM/LbfY5MR4AHk5z/3HZOIjEnie
# DHYnSY+arA504wZVVUnI7aF8cEVhfrJxFh7hwUG50tIy6VIk8zZQBNfdbzxJ1QvU
# dkD8ZWUTfpVROtX/uJqnV2tLFeU3WB/cAA3FrurfgUf58FKu5s9arOAUSqZxlID6
# /bAjMGDpg2CsDiQe/xHy56VVYpXun3+eKdbNSwp2g/BDBN8GSSDyU1pEsFF6OQID
# AQABo4IBCTCCAQUwHQYDVR0OBBYEFM0ZrGFNlGcr9q+UdVnb8FgAg6E6MB8GA1Ud
# IwQYMBaAFCM0+NlSRnAK7UD7dvuzK7DDNbMPMFQGA1UdHwRNMEswSaBHoEWGQ2h0
# dHA6Ly9jcmwubWljcm9zb2Z0LmNvbS9wa2kvY3JsL3Byb2R1Y3RzL01pY3Jvc29m
# dFRpbWVTdGFtcFBDQS5jcmwwWAYIKwYBBQUHAQEETDBKMEgGCCsGAQUFBzAChjxo
# dHRwOi8vd3d3Lm1pY3Jvc29mdC5jb20vcGtpL2NlcnRzL01pY3Jvc29mdFRpbWVT
# dGFtcFBDQS5jcnQwEwYDVR0lBAwwCgYIKwYBBQUHAwgwDQYJKoZIhvcNAQEFBQAD
# ggEBAFEc1t82HdyAvAKnxpnfFsiQBmkVmjK582QQ0orzYikbeY/KYKmzXcTkFi01
# jESb8fRcYaRBrpqLulDRanlqs2KMnU1RUAupjtS/ohDAR9VOdVKJHj+Wao8uQBQG
# cu4/cFmSXYXtg5n6goSe5AMBIROrJ9bMcUnl2h3/bzwJTtWNZugMyX/uMRQCN197
# aeyJPkV/JUTnHxrWxRrDSuTh8YSY50/5qZinGEbshGzsqQMK/Xx6Uh2ca6SoD5iS
# pJJ4XCt4432yx9m2cH3fW3NTv6rUZlBL8Mk7lYXlwUplnSVYULsgVJF5OhsHXGpX
# KK8xx5/nwx3uR/0n13/PdNxlxT8wggTsMIID1KADAgECAhMzAAAAsBGvCovQO5/d
# AAEAAACwMA0GCSqGSIb3DQEBBQUAMHkxCzAJBgNVBAYTAlVTMRMwEQYDVQQIEwpX
# YXNoaW5ndG9uMRAwDgYDVQQHEwdSZWRtb25kMR4wHAYDVQQKExVNaWNyb3NvZnQg
# Q29ycG9yYXRpb24xIzAhBgNVBAMTGk1pY3Jvc29mdCBDb2RlIFNpZ25pbmcgUENB
# MB4XDTEzMDEyNDIyMzMzOVoXDTE0MDQyNDIyMzMzOVowgYMxCzAJBgNVBAYTAlVT
# MRMwEQYDVQQIEwpXYXNoaW5ndG9uMRAwDgYDVQQHEwdSZWRtb25kMR4wHAYDVQQK
# ExVNaWNyb3NvZnQgQ29ycG9yYXRpb24xDTALBgNVBAsTBE1PUFIxHjAcBgNVBAMT
# FU1pY3Jvc29mdCBDb3Jwb3JhdGlvbjCCASIwDQYJKoZIhvcNAQEBBQADggEPADCC
# AQoCggEBAOivXKIgDfgofLwFe3+t7ut2rChTPzrbQH2zjjPmVz+lURU0VKXPtIup
# P6g34S1Q7TUWTu9NetsTdoiwLPBZXKnr4dcpdeQbhSeb8/gtnkE2KwtA+747urlc
# dZMWUkvKM8U3sPPrfqj1QRVcCGUdITfwLLoiCxCxEJ13IoWEfE+5G5Cw9aP+i/QM
# mk6g9ckKIeKq4wE2R/0vgmqBA/WpNdyUV537S9QOgts4jxL+49Z6dIhk4WLEJS4q
# rp0YHw4etsKvJLQOULzeHJNcSaZ5tbbbzvlweygBhLgqKc+/qQUF4eAPcU39rVwj
# gynrx8VKyOgnhNN+xkMLlQAFsU9lccUCAwEAAaOCAWAwggFcMBMGA1UdJQQMMAoG
# CCsGAQUFBwMDMB0GA1UdDgQWBBRZcaZaM03amAeA/4Qevof5cjJB8jBRBgNVHREE
# SjBIpEYwRDENMAsGA1UECxMETU9QUjEzMDEGA1UEBRMqMzE1OTUrNGZhZjBiNzEt
# YWQzNy00YWEzLWE2NzEtNzZiYzA1MjM0NGFkMB8GA1UdIwQYMBaAFMsR6MrStBZY
# Ack3LjMWFrlMmgofMFYGA1UdHwRPME0wS6BJoEeGRWh0dHA6Ly9jcmwubWljcm9z
# b2Z0LmNvbS9wa2kvY3JsL3Byb2R1Y3RzL01pY0NvZFNpZ1BDQV8wOC0zMS0yMDEw
# LmNybDBaBggrBgEFBQcBAQROMEwwSgYIKwYBBQUHMAKGPmh0dHA6Ly93d3cubWlj
# cm9zb2Z0LmNvbS9wa2kvY2VydHMvTWljQ29kU2lnUENBXzA4LTMxLTIwMTAuY3J0
# MA0GCSqGSIb3DQEBBQUAA4IBAQAx124qElczgdWdxuv5OtRETQie7l7falu3ec8C
# nLx2aJ6QoZwLw3+ijPFNupU5+w3g4Zv0XSQPG42IFTp8263Os8lsujksRX0kEVQm
# MA0N/0fqAwfl5GZdLHudHakQ+hywdPJPaWueqSSE2u2WoN9zpO9qGqxLYp7xfMAU
# f0jNTbJE+fA8k21C2Oh85hegm2hoCSj5ApfvEQO6Z1Ktwemzc6bSY81K4j7k8079
# /6HguwITO10g3lU/o66QQDE4dSheBKlGbeb1enlAvR/N6EXVruJdPvV1x+ZmY2DM
# 1ZqEh40kMPfvNNBjHbFCZ0oOS786Du+2lTqnOOQlkgimiGaCMIIFvDCCA6SgAwIB
# AgIKYTMmGgAAAAAAMTANBgkqhkiG9w0BAQUFADBfMRMwEQYKCZImiZPyLGQBGRYD
# Y29tMRkwFwYKCZImiZPyLGQBGRYJbWljcm9zb2Z0MS0wKwYDVQQDEyRNaWNyb3Nv
# ZnQgUm9vdCBDZXJ0aWZpY2F0ZSBBdXRob3JpdHkwHhcNMTAwODMxMjIxOTMyWhcN
# MjAwODMxMjIyOTMyWjB5MQswCQYDVQQGEwJVUzETMBEGA1UECBMKV2FzaGluZ3Rv
# bjEQMA4GA1UEBxMHUmVkbW9uZDEeMBwGA1UEChMVTWljcm9zb2Z0IENvcnBvcmF0
# aW9uMSMwIQYDVQQDExpNaWNyb3NvZnQgQ29kZSBTaWduaW5nIFBDQTCCASIwDQYJ
# KoZIhvcNAQEBBQADggEPADCCAQoCggEBALJyWVwZMGS/HZpgICBCmXZTbD4b1m/M
# y/Hqa/6XFhDg3zp0gxq3L6Ay7P/ewkJOI9VyANs1VwqJyq4gSfTwaKxNS42lvXlL
# cZtHB9r9Jd+ddYjPqnNEf9eB2/O98jakyVxF3K+tPeAoaJcap6Vyc1bxF5Tk/TWU
# cqDWdl8ed0WDhTgW0HNbBbpnUo2lsmkv2hkL/pJ0KeJ2L1TdFDBZ+NKNYv3LyV9G
# MVC5JxPkQDDPcikQKCLHN049oDI9kM2hOAaFXE5WgigqBTK3S9dPY+fSLWLxRT3n
# rAgA9kahntFbjCZT6HqqSvJGzzc8OJ60d1ylF56NyxGPVjzBrAlfA9MCAwEAAaOC
# AV4wggFaMA8GA1UdEwEB/wQFMAMBAf8wHQYDVR0OBBYEFMsR6MrStBZYAck3LjMW
# FrlMmgofMAsGA1UdDwQEAwIBhjASBgkrBgEEAYI3FQEEBQIDAQABMCMGCSsGAQQB
# gjcVAgQWBBT90TFO0yaKleGYYDuoMW+mPLzYLTAZBgkrBgEEAYI3FAIEDB4KAFMA
# dQBiAEMAQTAfBgNVHSMEGDAWgBQOrIJgQFYnl+UlE/wq4QpTlVnkpDBQBgNVHR8E
# STBHMEWgQ6BBhj9odHRwOi8vY3JsLm1pY3Jvc29mdC5jb20vcGtpL2NybC9wcm9k
# dWN0cy9taWNyb3NvZnRyb290Y2VydC5jcmwwVAYIKwYBBQUHAQEESDBGMEQGCCsG
# AQUFBzAChjhodHRwOi8vd3d3Lm1pY3Jvc29mdC5jb20vcGtpL2NlcnRzL01pY3Jv
# c29mdFJvb3RDZXJ0LmNydDANBgkqhkiG9w0BAQUFAAOCAgEAWTk+fyZGr+tvQLEy
# tWrrDi9uqEn361917Uw7LddDrQv+y+ktMaMjzHxQmIAhXaw9L0y6oqhWnONwu7i0
# +Hm1SXL3PupBf8rhDBdpy6WcIC36C1DEVs0t40rSvHDnqA2iA6VW4LiKS1fylUKc
# 8fPv7uOGHzQ8uFaa8FMjhSqkghyT4pQHHfLiTviMocroE6WRTsgb0o9ylSpxbZsa
# +BzwU9ZnzCL/XB3Nooy9J7J5Y1ZEolHN+emjWFbdmwJFRC9f9Nqu1IIybvyklRPk
# 62nnqaIsvsgrEA5ljpnb9aL6EiYJZTiU8XofSrvR4Vbo0HiWGFzJNRZf3ZMdSY4t
# vq00RBzuEBUaAF3dNVshzpjHCe6FDoxPbQ4TTj18KUicctHzbMrB7HCjV5JXfZSN
# oBtIA1r3z6NnCnSlNu0tLxfI5nI3EvRvsTxngvlSso0zFmUeDordEN5k9G/ORtTT
# F+l5xAS00/ss3x+KnqwK+xMnQK3k+eGpf0a7B2BHZWBATrBC7E7ts3Z52Ao0CW0c
# gDEf4g5U3eWh++VHEK1kmP9QFi58vwUheuKVQSdpw5OPlcmN2Jshrg1cnPCiroZo
# gwxqLbt2awAdlq3yFnv2FoMkuYjPaqhHMS+a3ONxPdcAfmJH0c6IybgY+g5yjcGj
# Pa8CQGr/aZuW4hCoELQ3UAjWwz0wggYHMIID76ADAgECAgphFmg0AAAAAAAcMA0G
# CSqGSIb3DQEBBQUAMF8xEzARBgoJkiaJk/IsZAEZFgNjb20xGTAXBgoJkiaJk/Is
# ZAEZFgltaWNyb3NvZnQxLTArBgNVBAMTJE1pY3Jvc29mdCBSb290IENlcnRpZmlj
# YXRlIEF1dGhvcml0eTAeFw0wNzA0MDMxMjUzMDlaFw0yMTA0MDMxMzAzMDlaMHcx
# CzAJBgNVBAYTAlVTMRMwEQYDVQQIEwpXYXNoaW5ndG9uMRAwDgYDVQQHEwdSZWRt
# b25kMR4wHAYDVQQKExVNaWNyb3NvZnQgQ29ycG9yYXRpb24xITAfBgNVBAMTGE1p
# Y3Jvc29mdCBUaW1lLVN0YW1wIFBDQTCCASIwDQYJKoZIhvcNAQEBBQADggEPADCC
# AQoCggEBAJ+hbLHf20iSKnxrLhnhveLjxZlRI1Ctzt0YTiQP7tGn0UytdDAgEesH
# 1VSVFUmUG0KSrphcMCbaAGvoe73siQcP9w4EmPCJzB/LMySHnfL0Zxws/HvniB3q
# 506jocEjU8qN+kXPCdBer9CwQgSi+aZsk2fXKNxGU7CG0OUoRi4nrIZPVVIM5AMs
# +2qQkDBuh/NZMJ36ftaXs+ghl3740hPzCLdTbVK0RZCfSABKR2YRJylmqJfk0waB
# SqL5hKcRRxQJgp+E7VV4/gGaHVAIhQAQMEbtt94jRrvELVSfrx54QTF3zJvfO4OT
# oWECtR0Nsfz3m7IBziJLVP/5BcPCIAsCAwEAAaOCAaswggGnMA8GA1UdEwEB/wQF
# MAMBAf8wHQYDVR0OBBYEFCM0+NlSRnAK7UD7dvuzK7DDNbMPMAsGA1UdDwQEAwIB
# hjAQBgkrBgEEAYI3FQEEAwIBADCBmAYDVR0jBIGQMIGNgBQOrIJgQFYnl+UlE/wq
# 4QpTlVnkpKFjpGEwXzETMBEGCgmSJomT8ixkARkWA2NvbTEZMBcGCgmSJomT8ixk
# ARkWCW1pY3Jvc29mdDEtMCsGA1UEAxMkTWljcm9zb2Z0IFJvb3QgQ2VydGlmaWNh
# dGUgQXV0aG9yaXR5ghB5rRahSqClrUxzWPQHEy5lMFAGA1UdHwRJMEcwRaBDoEGG
# P2h0dHA6Ly9jcmwubWljcm9zb2Z0LmNvbS9wa2kvY3JsL3Byb2R1Y3RzL21pY3Jv
# c29mdHJvb3RjZXJ0LmNybDBUBggrBgEFBQcBAQRIMEYwRAYIKwYBBQUHMAKGOGh0
# dHA6Ly93d3cubWljcm9zb2Z0LmNvbS9wa2kvY2VydHMvTWljcm9zb2Z0Um9vdENl
# cnQuY3J0MBMGA1UdJQQMMAoGCCsGAQUFBwMIMA0GCSqGSIb3DQEBBQUAA4ICAQAQ
# l4rDXANENt3ptK132855UU0BsS50cVttDBOrzr57j7gu1BKijG1iuFcCy04gE1CZ
# 3XpA4le7r1iaHOEdAYasu3jyi9DsOwHu4r6PCgXIjUji8FMV3U+rkuTnjWrVgMHm
# lPIGL4UD6ZEqJCJw+/b85HiZLg33B+JwvBhOnY5rCnKVuKE5nGctxVEO6mJcPxaY
# iyA/4gcaMvnMMUp2MT0rcgvI6nA9/4UKE9/CCmGO8Ne4F+tOi3/FNSteo7/rvH0L
# QnvUU3Ih7jDKu3hlXFsBFwoUDtLaFJj1PLlmWLMtL+f5hYbMUVbonXCUbKw5TNT2
# eb+qGHpiKe+imyk0BncaYsk9Hm0fgvALxyy7z0Oz5fnsfbXjpKh0NbhOxXEjEiZ2
# CzxSjHFaRkMUvLOzsE1nyJ9C/4B5IYCeFTBm6EISXhrIniIh0EPpK+m79EjMLNTY
# MoBMJipIJF9a6lbvpt6Znco6b72BJ3QGEe52Ib+bgsEnVLaxaj2JoXZhtG6hE6a/
# qkfwEm/9ijJssv7fUciMI8lmvZ0dhxJkAj0tr1mPuOQh5bWwymO0eFQF1EEuUKyU
# sKV4q7OglnUa2ZKHE3UiLzKoCG6gW4wlv6DvhMoh1useT8ma7kng9wFlb4kLfchp
# yOZu6qeXzjEp/w7FW1zYTRuh2Povnj8uVRZryROj/TGCBI4wggSKAgEBMIGQMHkx
# CzAJBgNVBAYTAlVTMRMwEQYDVQQIEwpXYXNoaW5ndG9uMRAwDgYDVQQHEwdSZWRt
# b25kMR4wHAYDVQQKExVNaWNyb3NvZnQgQ29ycG9yYXRpb24xIzAhBgNVBAMTGk1p
# Y3Jvc29mdCBDb2RlIFNpZ25pbmcgUENBAhMzAAAAsBGvCovQO5/dAAEAAACwMAkG
# BSsOAwIaBQCggbAwGQYJKoZIhvcNAQkDMQwGCisGAQQBgjcCAQQwHAYKKwYBBAGC
# NwIBCzEOMAwGCisGAQQBgjcCARUwIwYJKoZIhvcNAQkEMRYEFFBDOjk/emNHMZT1
# P/+oO2RnNhndMFAGCisGAQQBgjcCAQwxQjBAoCKAIABFAG4AdABpAHQAeQAgAEYA
# cgBhAG0AZQB3AG8AcgBroRqAGGh0dHA6Ly9tc2RuLmNvbS9kYXRhL2VmIDANBgkq
# hkiG9w0BAQEFAASCAQDgYhnIDKloOR+xIYEZs8gHHGpsLveWUIzcplKikvP8qEpA
# wfxIGbBlKVdMbegetdNCXjp1693blqScWOeWkCgHHN3Um+NKA5YQmDPBb5oyGnIu
# U0dXVoLsbxgFW1kGl3nwIIGO9FupcmGYIp/KO1teZwPtEUCTAyhTvYhNPdTZ8C68
# Y+u/oAap3zPRvV01oXzCEMeCmdE+1PACq5zvJVwkArvn8z1P5Yss/Edb+7Afm3sU
# jdqxg2Xe3yf8nXyReoa80vAGVojsGwgvztvI8dElKmGSFUFJH71I+LcHa/FpYTr/
# XwDbPR2PD7GEP0IMbdH0YqESFFmLP8KOl8o95UsAoYICHzCCAhsGCSqGSIb3DQEJ
# BjGCAgwwggIIAgEBMIGFMHcxCzAJBgNVBAYTAlVTMRMwEQYDVQQIEwpXYXNoaW5n
# dG9uMRAwDgYDVQQHEwdSZWRtb25kMR4wHAYDVQQKExVNaWNyb3NvZnQgQ29ycG9y
# YXRpb24xITAfBgNVBAMTGE1pY3Jvc29mdCBUaW1lLVN0YW1wIFBDQQIKYQKSSgAA
# AAAAIDAJBgUrDgMCGgUAoF0wGAYJKoZIhvcNAQkDMQsGCSqGSIb3DQEHATAcBgkq
# hkiG9w0BCQUxDxcNMTMwMjIyMTkxMTA2WjAjBgkqhkiG9w0BCQQxFgQUmW7sRqG8
# eSOCjAFX0gZ7T1shm28wDQYJKoZIhvcNAQEFBQAEggEAo0K+BgNxnEtg02oxycUT
# 442aVruVBTf8egY0GFthF4jgMdegRW/+e1U4jpOfONuNKEe2sdvB6jwPJ9ZQy2W6
# kwnSQ8JrWXUgdnLAuDRYsCp1X4xO/HfsfX9i8WLWJVgUasgmKAkaywCdazeeF6YD
# vtGVGNCXnxdvtmTmhVgICU0099NOEcs7M/nU3puc7qCt9ozwJYeeGQrG6VFQEzye
# HHN0Ooumxjlkro5w6OWNV4ASkc4zpz5JWfGGCe7aw+KV5J+FapyeMXFykdG35MK8
# RlAKqatsLQRWVpyGbHVEEn6UIbgxqJAuQtS7BRApgzOg6M3HNFxnIZcJ8QkVc74h
# Jg==
# SIG # End signature block
