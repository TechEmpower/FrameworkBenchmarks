<Project Sdk="Microsoft.NET.Sdk.Web">

   <PropertyGroup>
      <TargetFramework>net6.0</TargetFramework>
      <OutputType>Exe</OutputType>
      <AllowUnsafeBlocks>true</AllowUnsafeBlocks>

      <TrimmerDefaultAction>link</TrimmerDefaultAction>
      <IlcOptimizationPreference>Speed</IlcOptimizationPreference>
      <IlcPgoOptimize>true</IlcPgoOptimize>
      <IlcTrimMetadata>true</IlcTrimMetadata>

      <UseSystemResourceKeys>true</UseSystemResourceKeys>
      <EventSourceSupport>false</EventSourceSupport>
      <DebuggerSupport>false</DebuggerSupport>
      <IlcGenerateStackTraceData>false</IlcGenerateStackTraceData>
   </PropertyGroup>

   <ItemGroup>
      <PackageReference Include="Npgsql" Version="6.0.2" />
      <PackageReference Include="Microsoft.DotNet.ILCompiler" Version="7.0.0-*" />
   </ItemGroup>

   <PropertyGroup>
      <DefineConstants>$(DefineConstants);ADO</DefineConstants>
   </PropertyGroup>

</Project>