var usern;
function login()
{
	var obj = new Array();
	var opt = {
	    method: 'post',
	    postBody: $('login-form').serialize(),
	    onSuccess: function(response) 
		{
			var msg = response.responseText;
			if(msg=="Valid Login")
			{
				
				$('login-form').style.display = "none";
				usern = $('login-form').username.value;
				document.body.innerHTML = ("Welcome to the Homepage<input type=\"checkbox\" value=\"Google\" onclick=\"requestGoogleToken()\"/>");
			}
			else
			{
				$('login-form').username.value = "";
				$('login-form').password.value = "";
				alert(msg);
			}	
	   	}
	}
	new Ajax.Request('/oauthApp/login.auth', opt);
}

function requestGoogleToken()
{
	var opt = {
	    method: 'get',
	    parameters: {tusername: usern},
	    onSuccess: function(response) 
		{
			var msg = response.responseText;
			if(msg=="Acquired request token")
			{
				alert(msg);
				document.body.innerHTML = ("<a href='/oauthApp/authorizeUser.auth?tusername="+usern+"'>Click</a>");
				//authorizeGoogle();
			}
			else
			{
				alert(msg);
			}	
	   	}
	}
	new Ajax.Request('/oauthApp/requestToken.auth', opt);
}

function authorizeGoogle()
{
	var opt = {
		method: 'get',
		parameters: {tusername: usern},
	    onSuccess: function(response) 
		{
			var msg = response.responseText;
			if(msg=="Request Validated")
			{
				alert(msg);
			}
			else
			{
				alert(msg);
			}	
	   	}
	}
	new Ajax.Request('/oauthApp/authorizeUser.auth', opt);
}

function getResource(id,user)
{
	var file = document.getElementById(id).value;
	usern = user;
	document.body.innerHTML += "<a href='/oauthApp/getResource.auth?tusername="+user+"&file="+file+"'>"+file+"</a>";
	/*var opt = {
		method: 'get',
		parameters: {tusername: usern,file: document.getElementById(id).value},
	    onSuccess: function(response) 
		{
			var msg = response.responseText;
			if(msg=="Got Resource")
			{
				alert(msg);
			}
			else
			{
				alert(msg);
			}	
	   	}
	}
	new Ajax.Request('/oauthApp/getResource.auth', opt);*/
}