
var _fview_namespace = {
"_fview_cntxt_global_js_callback0" : function(response){alert(response.responseText)},
"_fview_cntxt_global_js_callback1" : function(response){document.getElementById('para').innerHTML=response;alert(response.responseText)}
}


window.onload = function(){
document.getElementById('text').onclick = function(){TestPage.textonclick(1,document.getElementById('link').innerText,'Hello',"_fview_cntxt_global_js_callback0","/default/default/test.fview",_fview_namespace);}
document.getElementById('link').onclick = function(){TestPage.linkonclick("_fview_cntxt_global_js_callback1","/default/default/test.fview",_fview_namespace);}
}


				function test()
				{
					alert("Hello />");
				}
			