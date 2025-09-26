<cfsetting enablecfoutputonly=true>
<cfheader name="Server" value="cfml-lucee">
<cfheader name="Content-Type" value="application/json">
<cfoutput>#serializeJSON( { 'message' : 'Hello, World!' } )#</cfoutput>