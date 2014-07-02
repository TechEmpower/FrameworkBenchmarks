# from Nimrod's Runtime Library - xmltree
# (c) Copyright 2012 Andreas Rumpf
# Modifications by Erwan Ameil

proc addEscaped*(result: var string, s: string) = 
  ## same as ``result.add(escape(s))``, but more efficient.
  for c in items(s):
    case c
    of '<': result.add("&lt;")
    of '>': result.add("&gt;")
    of '&': result.add("&amp;")
    of '"': result.add("&quot;")
    of '\'': result.add("&#x27;")
    of '/': result.add("&#x2F;")
    else: result.add(c)

proc escape*(s: string): string = 
  ## escapes `s` for inclusion into an XML document. 
  ## Escapes these characters:
  ##
  ## ------------    -------------------
  ## char            is converted to
  ## ------------    -------------------
  ##  ``<``          ``&lt;``
  ##  ``>``          ``&gt;``
  ##  ``&``          ``&amp;``
  ##  ``"``          ``&quot;``
  ##  ``'``          ``&#x27;``
  ##  ``/``          ``&#x2F;``
  ## ------------    -------------------
  result = newStringOfCap(s.len)
  addEscaped(result, s)
 
