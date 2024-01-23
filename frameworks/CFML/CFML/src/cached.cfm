<cfsetting enablecfoutputonly=true>
<cfset cache = application.cachebox.getCache( 'default' )>
<cfparam name="url.count" default="1">
<cfset url.count = val( url.count )>
<cfif url.count gt 500 >
    <cfset url.count = 500>
<cfelseif url.count lt 1 >
    <cfset url.count = 1>
</cfif>
<cfheader name="Server" value="cfml-lucee">
<cfheader name="Content-Type" value="application/json">
<cfset results = []>
<cfloop from="1" to="#url.count#" index="i">
    <cfset id = randRange( 1, 10000 )>
    <cfset qry = cache.getOrSet(
        'cached-world-#id#',
        ()=>queryExecute( '
            SELECT id, randomNumber
            FROM World
            WHERE id = #id#
        ').getRow( 1 ) ) >    
    <cfset results.append( qry )>
</cfloop>
<cfoutput>#serializeJSON( results )#</cfoutput>