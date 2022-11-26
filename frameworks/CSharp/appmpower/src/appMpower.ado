<Project Sdk="Microsoft.NET.Sdk.Web">

   <PropertyGroup>
      <TargetFramework>net7.0</TargetFramework>
      <OutputType>Exe</OutputType>
      <AllowUnsafeBlocks>true</AllowUnsafeBlocks>

      <!-- Normal .NET 7 -->
      <PublishAot>true</PublishAot>
      <SelfContained>true</SelfContained>
      <TrimmerDefaultAction>link</TrimmerDefaultAction>
      <InvariantGlobalization>true</InvariantGlobalization>
      <IlcGenerateStackTraceData>false</IlcGenerateStackTraceData>
      <IlcOptimizationPreference>Speed</IlcOptimizationPreference>
      <DebugType>none</DebugType>
      <GenerateRuntimeConfigurationFiles>false</GenerateRuntimeConfigurationFiles>

      <!-- Only some may work - From the experimental AOT version -->
      <IlcFoldIdenticalMethodBodies>true</IlcFoldIdenticalMethodBodies>
      <IlcTrimMetadata>true</IlcTrimMetadata>
      <IlcInvariantGlobalization>true</IlcInvariantGlobalization>
      <IlcGenerateCompleteTypeMetadata>false</IlcGenerateCompleteTypeMetadata>

      <!-- Still works from the experimental AOT version, but high risk -->
      <IlcDisableReflection>true</IlcDisableReflection>

      <SuppressTrimAnalysisWarnings>true</SuppressTrimAnalysisWarnings>

      <!--
      <TrimMode>link</TrimMode>
      -->

      <!-- Opt out of the "easy mode" of the CoreRT compiler (http://aka.ms/OptimizeCoreRT) -->
      <IlcPgoOptimize>true</IlcPgoOptimize>

      <!-- This benchmark is marked Stripped, so we might as well do this: -->
      <UseSystemResourceKeys>true</UseSystemResourceKeys>
      <EventSourceSupport>false</EventSourceSupport>
      <DebuggerSupport>false</DebuggerSupport>
   </PropertyGroup>

   <ItemGroup>
      <PackageReference Include="Npgsql" Version="7.0.0" />
      <PackageReference Include="System.Data.SqlClient" Version="4.8.5" />
      <PackageReference Include="System.Data.Odbc" Version="7.0.0" />
      <PackageReference Include="Microsoft.DotNet.ILCompiler" Version="7.0.0-*" />
   </ItemGroup>

   <PropertyGroup>
      <DefineConstants>$(DefineConstants);ADO</DefineConstants>
      <DefineConstants>$(DefineConstants);POSTGRESQL</DefineConstants>
   </PropertyGroup>

</Project>