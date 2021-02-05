<cfsetting enablecfoutputonly=true>
<cfparam name="url.queries" default="1">
<cfset url.queries = val( url.queries )>
<cfif url.queries gt 500 >
    <cfset url.queries = 500>
<cfelseif url.queries lt 1 >
    <cfset url.queries = 1>
</cfif>
<cfheader name="Server" value="cfml-lucee">
<cfheader name="Content-Type" value="application/json">
<cfset results = []>
<cfloop times="#url.queries#">
    <cfquery name="qry" datasource="world" returntype="array">
        SELECT id, randomNumber
        FROM World
        WHERE id = <cfqueryparam value="#randRange( 1, 10000 )#" cfsqltype="integer">
    </cfquery>
    <cfset results.append( { 'id' : qry[ 1 ].id, 'randomNumber' : randRange( 1, 1000 ) } )>
</cfloop>
<cfquery datasource="world">
    update World as w set
        randomNumber = w2.randomNumber
    from (values
        #results.map( (r)=>"(#val( r.id )#, #val( r.randomNumber )#)" ).toList( ',' )#        
        ) as w2(id,randomNumber)
    where w2.id = w.id;
</cfquery>
<cfoutput>#serializeJSON( results )#</cfoutput>