<cfsetting enablecfoutputonly=true>
<cfheader name="Server" value="cfml-lucee">
<cfheader name="Content-Type" value="text/html; charset=utf-8">
<cfquery name="qry">
    SELECT id, message
    FROM Fortune
</cfquery>
<cfset qry.addRow( { 'id' : 0, 'message' : 'Additional fortune added at request time.' } )>
<cfset qry = qry.sort( (a,b)=>compareNoCase(a.message,b.message) )>
<cfoutput><!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>
<cfloop query="#qry#"><tr><td>#qry.id#</td><td>#htmlEditFormat( qry.message )#</td></tr></cfloop>
</table></body></html>
</cfoutput>