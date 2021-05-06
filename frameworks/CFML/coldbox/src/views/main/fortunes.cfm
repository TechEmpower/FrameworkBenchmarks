<cfoutput><!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>
<cfloop query="#prc.qry#"><tr><td>#prc.qry.id#</td><td>#htmlEditFormat( prc.qry.message )#</td></tr></cfloop>
</table></body></html>
</cfoutput>