/**
 * Created by dsolimando on 01/11/2018.
 */

show.scale()

rest.get("/json").then {
    [message:'Hello, World!']
}

rest.get('/plaintext').then {
    new hot.Response(200,['Content-Type':'text/plain'],'Hello, World!')
}