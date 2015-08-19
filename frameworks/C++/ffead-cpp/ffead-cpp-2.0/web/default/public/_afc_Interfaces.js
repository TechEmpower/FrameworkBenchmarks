var Expose= {
sayHello: function(_1,_2,_3,_cb,_url,_cntxt){
AfcCall("Expose","sayHello",new Array(_1,_2,_3),_cb,(_url==null?"/default/expose":_url),_cntxt);
}
,sayHello1: function(_1,_2,_3,_cb,_url,_cntxt){
AfcCall("Expose","sayHello1",new Array(_1,_2,_3),_cb,(_url==null?"/default/expose":_url),_cntxt);
}
,sayHello2: function(_1,_2,_3,_cb,_url,_cntxt){
AfcCall("Expose","sayHello2",new Array(JSON.stringify(_1),_2,_3),_cb,(_url==null?"/default/expose":_url),_cntxt);
}
};
var TestPage= {
textonclick: function(_1,_2,_3,_cb,_url,_cntxt){
AfcCall("TestPage","textonclick",new Array(_1,_2,_3),_cb,(_url==null?"":_url),_cntxt);
}
,linkonclick: function(_cb,_url,_cntxt){
AfcCall("TestPage","linkonclick",new Array(),_cb,(_url==null?"":_url),_cntxt);
}
};
