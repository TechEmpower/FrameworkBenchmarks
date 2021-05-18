component extends="coldbox.system.EventHandler" {
	property name='cache' inject='cachebox:default';

	function prehandler( event ) {
		event.setHTTPHeader( name="Server", value="cfml-coldbox" )
			.noLayout();
	}

	function json( event, rc, prc ) {
		event.renderData( data={ 'message' : 'Hello, World!' }, type="json" );
	}

	function db( event, rc, prc ) {
		var qry = queryExecute( '
			SELECT id, randomNumber
			FROM World
			WHERE id = #randRange( 1, 10000 )#
		');
		event.renderData( data=qry.getRow( 1 ), type="json" );
	}

	function queries( event, rc, prc ) {
				
		event.paramValue( 'queries', 1 );
		rc.queries = val( rc.queries );
		if( rc.queries > 500 ) {
			rc.queries = 500;
		} else if( rc.queries < 1 ) {
			rc.queries = 1;
		}
		var results = [];
		cfloop( from="1", to="#rc.queries#", index="local.i" ) {
			var qry = queryExecute( '
				SELECT id, randomNumber
				FROM World
				WHERE id = #randRange( 1, 10000 )#
			');
			results.append( qry.getRow( 1 ) );
		}

		event.renderData( data=results, type="json" );		
	}

	function fortunes( event, rc, prc ) {
		var qry = queryExecute( '
			SELECT id, message
			FROM Fortune
		');
		qry.addRow( { 'id' : 0, 'message' : 'Additional fortune added at request time.' } );
		prc.qry = qry.sort( (a,b)=>compareNoCase(a.message,b.message) );
		event.setView( "main/fortunes" );
	}

	function plaintext( event, rc, prc ) {
		event.renderData( data="Hello, World!", type="plain", contentType="text/plain" );
	}

	function updates( event, rc, prc ) {
		event.paramValue( 'queries', 1 );
		rc.queries = val( rc.queries );
		if( rc.queries > 500 ) {
			rc.queries = 500;
		} else if( rc.queries < 1 ) {
			rc.queries = 1;
		}
		var results = [];

		cfloop( from="1", to="#rc.queries#", index="local.i" ) {
			var qry = queryExecute( '
				SELECT id, randomNumber
				FROM World
				WHERE id = #randRange( 1, 10000 )#
			');
			results.append( { 'id' : qry.id, 'randomNumber' : randRange( 1, 1000 ) } );
		}
				
		cfloop( array="#results#", index="local.i" ) {
			queryExecute( '
				update World 
				SET randomNumber = #val( i.randomNumber )#
				where id = #val( i.id )#;
			');
		}

		event.renderData( data=results, type="json" );
	}

	function cached( event, rc, prc ) {
				
		event.paramValue( 'count', 1 );
		rc.count = val( rc.count );
		if( rc.count > 500 ) {
			rc.count = 500;
		} else if( rc.count < 1 ) {
			rc.count = 1;
		}
		var results = [];
		cfloop( from="1", to="#rc.count#", index="local.i" ) {
			var id = randRange( 1, 10000 );
			var qry = cache.getOrSet(
				'cached-world-#local.id#',
				()=>queryExecute( '
					SELECT id, randomNumber
					FROM World
					WHERE id = #id#
				').getRow( 1 ) );			
			results.append( qry );
		}

		event.renderData( data=results, type="json" );		
	}

}
