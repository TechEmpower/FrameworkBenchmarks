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
<cfloop from="1" to="#url.queries#" index="i">
    <cfquery name="qry">
        SELECT id, randomNumber
        FROM World
        WHERE id = <cfqueryparam value="#randRange( 1, 10000 )#" cfsqltype="integer">
    </cfquery>
    <cfset results.append( qry.getRow( 1 ) )>
</cfloop>
<cfoutput>#serializeJSON( results )#</cfoutput>