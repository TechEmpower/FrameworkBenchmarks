return {
	extensions = {
		{octopusExtensionsDir, "core"}, 
		{octopusExtensionsDir, "baseline"}, 
		{octopusExtensionsDir, "orm"}, 
		{octopusExtensionsDir, "app"},
	},
	
	octopusExtensionsDir = octopusExtensionsDir,
	octopusHostDir = octopusHostDir,
	port = 8080,
	securePort = 38080,
	luaCodeCache = "on",
	serverName = "tfb-server",
	errorLog = "error_log logs/error.log;",
	accessLog = "access_log logs/access.log;",
	includeDrop = [[#include drop.conf;]],
	maxBodySize = "50k",
	minifyJavaScript = false,
	minifyCommand = [[java -jar ../yuicompressor-2.4.8.jar %s -o %s]],
	
	databaseConnection = {
		rdbms       =   "mysql",
		host        =   "DBHOSTNAME",
		port        =   3306, 
		database    =   "hello_world",
		user        =   "benchmarkdbuser",
		password    =   "benchmarkdbpass",
		compact     =   false
	},

	globalParameters = {
		octopusHostDir = octopusHostDir,
		sourceCtxPath = "",
		requireSecurity = false,
		sessionTimeout = 3600,
		usePreparedStatement = false,
	},
}
