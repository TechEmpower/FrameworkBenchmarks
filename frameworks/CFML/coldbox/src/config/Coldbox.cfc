component {

	function configure() {

		coldbox = {
			appName                  : "ColdBox MVC",
			reinitPassword           : "",
			handlersIndexAutoReload  : false,
			customErrorTemplate      : "/coldbox/system/exceptions/Whoops.cfm",
			handlerCaching           : true
		};

		flash = {
			scope = "Mock"
		};

	}

}
