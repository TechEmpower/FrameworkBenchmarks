component{
	
	/**
	* Configure CacheBox for ColdBox Application Operation
	*/
	function configure(){
		
		// The CacheBox configuration structure DSL
		cacheBox = {
			// LogBox config already in coldbox app, not needed
			// logBoxConfig = "coldbox.system.web.config.LogBox", 
			
			// The defaultCache has an implicit name "default" which is a reserved cache name
			// It also has a default provider of cachebox which cannot be changed.
			// All timeouts are in minutes
			defaultCache = {
				objectDefaultTimeout = 120, //two hours default
				objectDefaultLastAccessTimeout = 30, //30 minutes idle time
				useLastAccessTimeouts = false,
				reapFrequency = 15,
				freeMemoryPercentageThreshold = 0,
				evictionPolicy = "LRU",
				evictCount = 1,
				maxObjects = 15000,
				objectStore = "ConcurrentStore", //guaranteed objects
				coldboxEnabled = true
			},
			
			// Register all the custom named caches you like here
			caches = {}		
		};
	}	

}