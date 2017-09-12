param($action)

if (!$TROOT) {
  $TROOT = "C:\FrameworkBenchmarks\frameworks\CSharp\revenj"
}
if (!$IROOT) {
  $IROOT = "C:\FrameworkBenchmarks\installs"
}
if (!$DBHOST) {
  $DBHOST = "localhost"
}

Add-Type -AssemblyName System.IO.Compression.FileSystem

$msbuild = $Env:windir + "\Microsoft.NET\Framework64\v4.0.30319\MSBuild.exe"
$java=$Env:JAVA_HOME + "\bin\java"
$dslclc=$TROOT + "\dsl-clc.jar"
$httpZip=$TROOT + "\http-server.zip"
$sln=$TROOT + "\Revenj.Bench.sln"
$revenj=$TROOT + "\exe\Revenj.Http.exe"
$exe=$TROOT + "\exe\"
$config=$TROOT + "\exe\Revenj.Http.exe.config"

echo "Stopping existing Revenj.Http"
Stop-Process -Name "Revenj.Http*" -ErrorAction 'SilentlyContinue' | Out-Null

if ($action -eq 'start') {

	echo "Cleaning up..."
	If (Test-Path $TROOT/exe) {
	  rmdir $TROOT/exe -recurse -force
	}
	if (Test-Path $TROOT/dsl-clc.jar) {
	  rm $TROOT/dsl-clc.jar
	}
	if (Test-Path $TROOT/http-server.zip) {
	  rm $TROOT/http-server.zip
	}

	echo "Download DSL compiler client"
	$client = new-object System.Net.WebClient
	$client.DownloadFile( "https://github.com/ngs-doo/dsl-compiler-client/releases/download/1.8.2/dsl-clc.jar", $dslclc )

	echo "Download Revenj HTTP server"
	$client = new-object System.Net.WebClient
	$client.DownloadFile( "https://github.com/ngs-doo/revenj/releases/download/1.4.2/http-server.zip", $httpZip )

	echo "Unzipping HTTP server"
	[System.IO.Compression.ZipFile]::ExtractToDirectory($httpZip, $exe)

	echo "Compiling the server model and downloading DSL Platform compiler..."
	&$java -jar $dslclc temp=$TROOT/tmp/ force dsl=$TROOT/Revenj.Bench manual-json revenj.net=$TROOT/exe/ServerModel.dll no-prompt dependencies:revenj.net=$TROOT/exe compiler=$IROOT/dsl-compiler.exe

	echo "Compiling the benchmark project..."
	&$msbuild $sln /p:Configuration=Release /t:Rebuild

	echo "Copying the configuration template"
	$template = Get-Content $TROOT/Revenj.Http.exe.config
	$Utf8NoBomEncoding = New-Object System.Text.UTF8Encoding($False)
	[System.IO.File]::WriteAllText($config, $template.Replace("server=localhost", "server=" + $DBHOST), $Utf8NoBomEncoding)

	echo "Starting Revenj..."
	Start-Process $revenj
}
