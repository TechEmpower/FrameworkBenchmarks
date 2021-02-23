component {

	this.datasource = 'world';

    function onApplicationStart() {        
        application.cacheBox = new cachebox.system.cache.CacheFactory( 'config.CacheBox' );

        var cache = application.cacheBox.getCache( 'default' );
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