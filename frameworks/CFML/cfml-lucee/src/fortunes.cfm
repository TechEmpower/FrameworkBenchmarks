<cfsetting enablecfoutputonly=true>
<cfheader name="Server" value="cfml-lucee">
<cfheader name="Content-Type" value="text/html; charset=utf-8">
<cfquery name="qry" datasource="world" returntype="array">
    SELECT id, message
    FROM Fortune
</cfquery>
<cfset qry.append( { 'id' : 0, 'message' : 'Additional fortune added at request time.' } )>
<cfset qry = qry.sort( (a,b)=>compareNoCase(a.message,b.message) )>
<cfoutput><!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>
<cfloop array="#qry#" index="row"><tr><td>#row.id#</td><td>#htmlEditFormat( row.message )#</td></tr></cfloop>
</table></body></html>
</cfoutput>