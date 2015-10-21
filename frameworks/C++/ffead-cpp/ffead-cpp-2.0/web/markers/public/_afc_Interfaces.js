var MarkerExpose= {
sayHello: function(_1,_2,_3,_cb,_url,_cntxt){
AfcCall("MarkerExpose","sayHello",new Array(_1,_2,_3),_cb,(_url==null?"/markers/markAjax":_url),_cntxt);
}
,sayHello1: function(_1,_2,_3,_cb,_url,_cntxt){
AfcCall("MarkerExpose","sayHello1",new Array(_1,_2,_3),_cb,(_url==null?"/markers/markAjax":_url),_cntxt);
}
,sayHello2: function(_1,_2,_3,_cb,_url,_cntxt){
AfcCall("MarkerExpose","sayHello2",new Array(JSON.stringify(_1),_2,_3),_cb,(_url==null?"/markers/markAjax":_url),_cntxt);
}
};
