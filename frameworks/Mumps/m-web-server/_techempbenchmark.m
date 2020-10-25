%techempbenchmark ; Techempower plaintext and json tests;2020-08-31 09:30PM
 ;

te
 S ^%webhttp(0,"NOGZIP")=1
 D job^%webreq(8080,"",,,1)
 ;D start^%webreq(8080,"",,,,,1)
 Q

plaintext(RESULT,ARGS) ; [Public] GET /plaintext
 S RESULT("mime")="text/plain"
 S RESULT="Hello, World!"
 Q

json(RESULT,ARGS) ; [Public] GET /json
 S RESULT("mime")="application/json"
 ;S RESULT("headers","Temp")="Test"
 N MSG
 S MSG("message")="Hello, World!"
 D ENCODE^%webjson("MSG","RESULT")
 K MSG
 Q
