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
	
	function afterConfigurationLoad() {
		var cache = controller.getCacheBox().getCache( 'default' );
		cfloop( from="1", to="10000", index="local.i" ) {
			cache.getOrSet(
				'cached-world-#i#',
				()=>queryExecute( '
					SELECT id, randomNumber
					FROM World
					WHERE id = #i#
				').getRow( 1 )
			);
		}
	}

}
