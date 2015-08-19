function prepForm(arr)
{
	var form = $('form');
	form.innerHTML = '';
	for(var i=0;i<arr.length;i++) 
	{
		var inp = new Element('input', { 'type': 'hidden' ,'name': arr[i].name ,'value': arr[i].value});
		form.appendChild(inp);
	};
}

/******Login functionality Starts*******/
function prepLogin()
{
	var usname = $('userName').value;
	var uspasswd = $('userPassword').value;
	if (isNotNB(usname) && isNotNB(uspasswd)) 
	{
		prepForm(new Array({name:"j_password",value:usname},{name:"j_username",value:uspasswd}));
		login();
	}
}
function login() 
{
	//Element.update('loginMessage', 'Sending request ...');
	//Element.show('loginMessage');
	var opt = {
	    method: 'post',
		postBody: Form.serialize($('form')) + '&ajax=true',
	    onSuccess: function(response) 
		{
	        var msg = response.responseText;
			alert(msg);
	        if ("error:" == msg.substr(0, 6)) 
			{
	            //var fp = "<font color='red'>" + msg.substring(6, msg.length) + '</font><br><a href="forgetPwd.html">Forget Password?</a>';
	            //Element.update('loginMessage', fp);
	        } 
			else if ("url:" == msg.substr(0, 4)) 
			{
	            location.href = msg.substring(4, msg.length);
	        } 
			else if ("message:" == msg.substr(0, 8)) 
			{
	            prepTwoColumLayout(false);
	        }
	    }
	}
	new Ajax.Request('j_acegi_security_check', opt);
}

function loginOnEnter(event) 
{
    if ((event.which && event.which == 13) || (event.keyCode && event.keyCode == 13)) 
	{
        prepLogin()
        return false;
    } 
	else 
	{
        return true;
    }
}

/******Login functionality Ends*******/


/******Register functionality Starts*******/
function prepRegister()
{
	var regusname = $('regusname').value;
	var reguspass = $('reguspass').value;
	var reguscpass = $('reguscpass').value;
	var regmale = $('regmale').checked;
	var regfemale = $('regfemale').checked;
	var regemail = $('regemail').value;
	var regday = $('regday').value;
	var regmon = $('regmon').value;
	var regyear = $('regyear').value;
	if(isNotNB(regusname) && isNotNB(reguspass) && isNotNB(reguscpass) && isNotNB(regemail) && isNotNB(regday) && isNotNB(regmon) && isNotNB(regyear) && reguspass==reguscpass && (regmale || regfemale))
	{
		if(regmale)
			regmale = "Male";
		else
			regmale = "Female";
		regday = regday + "/" + regmon + "/" + regyear;
		register(regusname,reguspass,regemail,regmale,regday);
	}
}
function register(regusname,reguspass,regemail,regmale,regday)
{
	var obj = new Array();
	obj.push({key:"userName",value:regusname});
	obj.push({key:"userPassword",value:reguspass});
	obj.push({key:"userEmail",value:regemail});
	obj.push({key:"gender",value:regmale});
	obj.push({key:"dob",value:regday});
	AjaxRequestHandler.register(obj,registerCallback);
}
function registerCallback(output)
{
	if(output=="success")
	{
		var div = $('register-wind').parentNode;
		div.removeChild($('register-wind'));
		div.innerHTML = 'Succesfully Registered';
	}
}
/******Register functionality Ends*******/

var modelName = null;
function getData(model)
{
	modelName = model;
	var obj = new Array();
	obj.push({key:"model",value:model});
	obj.push({key:"userId",value:3});
	AjaxRequestHandler.getData(obj,dataCallback);
}
function dataCallback(output)
{
	var data = JSON.parse(output);
	if(data!=null)
	{
		if(modelName=='Scraps' && data.list.length>0) 
			paintScraps(data.list);
		else if(modelName=='Autobio' && data.list.length>0) 
			paintBiography(data.list);
		else if(modelName=='Friends' && data.list.length>0) 
			paintFriends(data.list);
	}
}

function saveData()
{
	var obj = new Array();
	obj.push({key:"model",value:"Users"});
	obj.push({key:"id",value:18});
	obj.push({key:"userEmail",value:"sumeet1@sdas.com"});
	obj.push({key:"userName",value:"sumeet1"});
	obj.push({key:"userPassword",value:"sumeet1"});
	AjaxRequestHandler.saveData(obj,callba);
}
function callba(output)
{
	alert(output);
}


var service = null;
function getMails(folder)
{
	service = "mail";
	var obj = {from:"joe@localhost",passwd:"asdf",folderName:folder};
	MailRequestHandler.receive(obj,mailCallback);
}
function mailCallback(output)
{
	var data = JSON.parse(output);
	if(data!=null)
	{
		if(service=='mail' && data.Mail.length>0) 
			paintMails(data.Mail);
	}
}



function isNotNB(val)
{
	if(val!=null && trim(val)!="")
		return true;
}

function trim(str)
{
	if(str!=null)
		return str.replace(/^\s+|\s+$/g, '');
}

function prepTwoColumLayout(flag)
{
	var table = $('top-login-bar');
	table.deleteRow(0);
	var row = table.insertRow(0);
	var cell = row.insertCell(0);
	cell.className = 'top-font';
	cell.style.height = "26px";
	cell.innerHTML = '&nbsp;&nbsp;&nbsp;&nbsp;<font color="#ed8449" >Welcome</font> , Amit Thite  &nbsp;&nbsp;';
	cell = row.insertCell(1);
	cell.innerHTML = '<select> <OPTION VALUE="0"/>  Go To <OPTION VALUE="1"/>  Jobs <OPTION VALUE="2"/>  Dating <OPTION VALUE="3"/>  Bollywood <OPTION VALUE="1"/>  Games <OPTION VALUE="2"/>  Email <OPTION VALUE="3"/>  Articles</select>&nbsp;&nbsp;&nbsp;';
	if(flag)
	{
		$('mainpage').className= '';
		$('mainpage').innerHTML = '<p>&nbsp;</p>';
		table = document.createElement('table');
		table.width = "100%";
		var row = table.insertRow(0);
		var cell = row.insertCell(0);
		cell.width = "2%";
		cell.style.verticalAlign = "top";
		cell = row.insertCell(1);
		cell.width = "18%";
		cell.id = "leftNavHolder";
		cell.style.verticalAlign = "top";
		cell.innerHTML = '';
		cell = row.insertCell(2);
		cell.width = "2%";
		cell = row.insertCell(3);
		cell.width = "76%";
		cell.id = "rightContentHolder";
		cell.style.verticalAlign = "top";
		cell.innerHTML = '';
		cell = row.insertCell(4);
		cell.width = "2%";
		$('mainpage').appendChild(table);
	}
}
function paintBiography(data)
{
	var temp = '<div class="shadowcontainer"><div class="innerdiv"><div class="pen-image"><div class="dairy-info">';
	for(var i=0;i<data.length;i++)
		temp += data[i].content; 
	$('change-area').innerHTML = temp + '</div></div></div></div>';
}
function paintFriends(data)
{
	$('rightContentHolder').innerHTML = '<div class="mid-cont-links"> <ul> <li><a id="write-link" href="#" class="toggLinks" style="background:#449ed0;" onmouseover="toggleLinkBehaviour(this,true,null);" onmouseout="toggleLinkBehaviour(this,false,null);" onclick="toggleLinkBehaviour(this,true,\'List\');"><span>List</span></a></li> <li><a class="toggLinks" href="#" style="background:#449ed0;" onmouseover="toggleLinkBehaviour(this,true,null);" onmouseout="toggleLinkBehaviour(this,false,null);" onclick="toggleLinkBehaviour(this,true,\'Updates\');"><span>Updates</span></a></li> <li><a class="toggLinks" href="#" style="background:#449ed0;" onmouseover="toggleLinkBehaviour(this,true,null);" onmouseout="toggleLinkBehaviour(this,false,null);" onclick="toggleLinkBehaviour(this,true,\'View\');"><span>View</span></a></li> <li><a class="toggLinks" href="#" style="background:#449ed0;" onmouseover="toggleLinkBehaviour(this,true,null);" onmouseout="toggleLinkBehaviour(this,false,null);" onclick="toggleLinkBehaviour(this,true,\'Birthdays\');"><span>Birthdays</span></a></li> </ul> </div> <div class="mid-cont"> <div class="midbl-top"><div class="midbr-top"> <p>&nbsp;</p> <div style="width:100%" id="change-area"></div><p>&nbsp;</p><p>&nbsp;</p><p>&nbsp;</p></div></div></div>';
	var temp = '<p>&nbsp;</p>';
	for(var i=0;i<data.length;i++)
		temp += '<div class="mid-head"> <a href="#">Friends :-</a> List </div> <p>&nbsp;</p> <center> <div class="friends-head"> <div class="mid-conttl"><div class="mid-conttr"> <p>&nbsp;</p> </div></div> </div> <div class="friends"> <div class="mid-contbl"><div class="mid-contbr"> <table width="100%" style="margin-left:10px;"><tr> <td width="15%" style="border:1px solid black;"> <img src="img/photo.gif"/ style="margin-top:3px;border:1px solid black;"></br> <b style="color:#449ed0;">Amit Thite</b></br> </td> <td width="75%" align="left" valign="bottom"> <div class="fnds-links"> <li><a href="#">Delete</a></li> <li><a href="#">veiw</a></li> <li><a href="#">Favoutite</a></li> <li><a href="#">Blocked List</a></li> </div> </td> </tr> </table> <p>&nbsp;</p> </div></div> </div> </center> <p>&nbsp;</p> <p>&nbsp;</p> <p>&nbsp;</p>'; 
	$('change-area').innerHTML = temp;
}
function paintScraps(data)
{
	var temp = '';
	for(var i=0;i<data.length;i++)
		temp += '<div class="scrap-cont"> <div class="left-head1"> <div class="midbl-top"><div class="midbr-top"> &nbsp; </div></div> </div> <table width="100%" valign="top"><tr> <td width="15%"> <img src="img/photo.gif"/ style="margin-top:3px;border:1px solid black;"></br> <b style="color:#449ed0;">Amit Thite</b></br> </td> <td width="75%" align="left"><div class="longlines">'+data[i].content+'</div></td> </tr> </table> </div> <p>&nbsp;</p>'; 
	$('change-area').innerHTML = temp;
}
var currMails = new Array();
var mclckd = false;
var mclckdw = false;
function paintMails(data)
{
	if($('leftNavHolder')==null)
		prepTwoColumLayout(true);
	var temp = '';
	$('top-menu').innerHTML = '';
	$('leftNavHolder').innerHTML = '<div class="mail-back"> <div class="midbl-top"><div class="midbr-top"><div class="midtl-top"><div class="midtr-top"> <p>&nbsp;</p> <center> <div class="mail-left-links"> <div class="whitebl"><div class="whitebr"><div class="whitetl"><div class="whitetr"> <ul> <li><a href="#" onclick="mailPage(\'compose\')">Compose Mail</a></li> <li><a href="#" onclick="getMails(\'inbox\')">Inbox (no)</a></li> <li><a href="#" onclick="getMails(\'sent\')">Sent Mail </a></li> <li><a href="#" onclick="getMails(\'drafts\')">Drafts</a></li> <li><a href="#" onclick="getMails(\'spam\')">Spam</a></li> <li><a href="#">Address Book</a></li> <li><a href="#" onclick="getMails(\'trash\')">Trash Mail</a></li> <li><a href="#">Invitation</a></li> </ul> </div></div></div></div> </div> <p>&nbsp;</p> <div class="mail-left-links"> <div class="whitebl"><div class="whitebr"><div class="whitetl"><div class="whitetr"> <ul> <li><a href="#">Highlight</a></li> <li><a href="#">Read </a></li> <li><a href="#">Unread</a></li> <li><a href="#">All Mails</a></li> </ul> </div></div></div></div> </div> </center> <p>&nbsp;</p> </div></div></div></div> </div> <p>&nbsp;</p> <p>&nbsp;</p> addvertize comes here';
	temp = '<div class="mail-back" id="mailMainAreaDiv"> <div class="midbl-top"><div class="midbr-top"><div class="midtl-top"><div class="midtr-top"> <p>&nbsp;</p> <div class="mail-right-top"> <div class="whitetl"><div class="whitetr"> <p class="mail-heading">Inbox</p> <p>&nbsp;</p><table style="width:100%"> <tr><td><button style="background:#449ed0;color:white;padding:2px;">Delete</button> <SELECT style="width:70px;border: 1px solid #449ed0;margin-left:3px;"> <OPTION VALUE=""/>  Move To <OPTION VALUE="1"/>  Trash <OPTION VALUE="2"/>  Draft <OPTION VALUE="3"/>  Sent Mail </select> <button style="background:#449ed0;color:white;padding:2px;">Move</button> <SELECT style="width:70px;border: 1px solid #449ed0;margin-left:3px;"> <OPTION VALUE=""/>  Mark as <OPTION VALUE="1"/>  Highlight <OPTION VALUE="2"/>  Read <OPTION VALUE="3"/>  Unread </select> <button style="background:#449ed0;color:white;padding:2px;">Mark</button>  </td><td align="right"> Search Mail: <input type="text"style="width:120px;border:1px solid #449ed0;"> <button style="background:#449ed0;color:white;padding:2px;">&nbsp;&nbsp;Go&nbsp;&nbsp;</button> </td></tr> </table></div></div> </div> <div class="mail-right-white" id="mailContentArea">';
	for(var i=0;i<data.length;i++)
	{
		temp += '<table onmousedown="paintsingMail('+i+',this);" class="mail-gray-back" onmouseover="this.style.background=\'#FFCC99\';this.style.cursor=\'pointer\'" onmouseout="this.style.background=\'white\';"><tr><td width="5px">&nbsp;</td><td width="30px"><input style="margin-left:5px;" type="checkbox"/></td><td align="left" width="200px"><p style="width:180px;overflow:hidden;">'+data[i].from+'</p></td><td align="left" width="100px">'+data[i].subject+'</td> <td style="float:right;">'+data[i].sentDate+'</td><td width="10px">&nbsp;</td></tr></table>'; 
		currMails.push(data[i]);
	}
	temp += '</div> <p>&nbsp;</p> <table style="width:100%;"> <tr><td width="12px">&nbsp;</td><td> <button style="background:#eaeaea;color:black;padding:2px;">Delete</button> <SELECT style="width:70px;border: 1px solid #cccccc;margin-left:3px;"> <OPTION VALUE=""/>  Move To <OPTION VALUE="1"/>  Trash <OPTION VALUE="2"/>  Draft <OPTION VALUE="3"/>  Sent Mail </select> <button style="background:#eaeaea;color:black;padding:2px;">Move</button> <SELECT style="width:70px;border: 1px solid #cccccc;margin-left:3px;"> <OPTION VALUE=""/>  Mark as <OPTION VALUE="1"/>  Highlight <OPTION VALUE="2"/>  Read <OPTION VALUE="3"/>  Unread </select> <button style="background:#eaeaea;color:black;padding:2px;">Mark</button>  </td> <td align="right"> Search Mail: <input type="text"style="width:120px;border:1px solid #cccccc;"> <button style="background:#eaeaea;color:black;padding:2px;">&nbsp;&nbsp;Go&nbsp;&nbsp;</button> </td><td width="10px">&nbsp;</td></tr> </table><p>&nbsp;</p> </div></div></div></div> </div>';
	 $('rightContentHolder').innerHTML = temp;
	 disable_selection('mailMainAreaDiv');
	//for(var i=0;i<data.length;i++)
	//	$('mail'+i).onclick = function(){};
}
function disable_selection(id)
{
 	var element = document.getElementById(id);
	element.onselectstart = function () { return false; } // ie
	element.onmousedown = function () { return false; } // mozilla
}
var curr_ele = null;
var curr_ele_no = null;
function paintsingMail(no,obj)
{
	curr_ele = obj;
	curr_ele_no = no;
	document.onmousemove = drag;
	document.onmouseup = drop;
}
function drag()
{
	if(curr_ele!=null)
		document.body.style.cursor = "move";
	else
		document.body.style.cursor = "default";
}
function isEqualsOrChild(chi)
{
	if(curr_ele!=null && chi!=null)
	{
		var all = new Array();
		if(curr_ele==chi)return true;
		for(var i=0;i<curr_ele.rows.length;i++)
		{
			all.push(curr_ele.rows[i]);
			for(var j=0;j<curr_ele.rows[i].cells.length;j++)
			{
				all.push(curr_ele.rows[i].cells[j]);
				for(var k=0;k<curr_ele.rows[i].cells[j].childNodes.length;k++)
					all.push(curr_ele.rows[i].cells[j].childNodes[k]);
			}
		}
		for(var i=0;i<all.length;i++)
			if(all[i]==chi)
				return true;
	}
	return false;
}
function drop(e)
{
	var tempobj = window.event? event.srcElement: e.target;
	document.onmouseup = null;
	document.onmousemove = null;
	document.body.style.cursor = "default";
	//log(curr_ele+'------'+tempobj+'----'+isEqualsOrChild(tempobj));
	if(!isEqualsOrChild(tempobj))
	{
		if(curr_ele!=null)
		{
			log(tempobj);
			curr_ele.style.background = "green";
			tempobj.style.cursor = "default";
			log('----greeen'+tempobj);
		}
	}
	else 
	{
		$('mailContentArea').innerHTML='<p class="mail-head"style="margin-left:10px;"> From : '+currMails[curr_ele_no].from+'<span></span> <span style="float:right;margin-right:5px;">'+currMails[curr_ele_no].sentDate+'</span> </p> <p class="mail-cont">'+currMails[curr_ele_no].content+'<p>&nbsp;</p>';
	}
	curr_ele = null;
}

function mailPage(action)
{
	$('rightContentHolder').innerHTML = '<div class="mail-back"> <div class="midbl-top"><div class="midbr-top"><div class="midtl-top"><div class="midtr-top"> <p>&nbsp;</p> <div class="mail-right-top"> <div class="whitetl"><div class="whitetr"> <p class="mail-heading">Compose Mail</p> <p>&nbsp;</p> <p style="float:right;"><button class="mail-button"><a href="#">Send</a></button> <button class="mail-button"><a href="#">Save</a></button> <button class="mail-button"><a href="#">Cancel</a></button></p><p>&nbsp;</p></div></div> </div> <div class="mail-right-white"> <p>&nbsp;</p> <table width="100%" valign="top"><tr><td width="2%"></td> <td width="64%"><table><tr> <td class="mail-head">To : </td> <td><input type="text"style="width:250px;border:1px solid #cccccc;"></td></tr><tr><td>&nbsp;</td></tr><tr><td class="mail-head">Cc : </td> <td><input type="text"style="width:250px;border:1px solid #cccccc;"></td></tr> <tr><td>&nbsp;</td></tr><tr><td class="mail-head">Bcc : </td><td><input type="text"style="width:250px;border:1px solid #cccccc;"></td></tr> <tr><td>&nbsp;</td></tr><tr><td class="mail-head">Subject : </td> <td><input type="text"style="width:250px;border:1px solid #cccccc;"></td></tr> <tr><td>&nbsp;</td></tr><tr><td><button class="mail-button"><a href="#">Attachment</a></button></td></tr><tr><td>&nbsp;</td></tr></table> </td> <td width="2%"></td> <td width="30%"> <div class="add-book-back"> <div class="midtl-top"><div class="midtr-top"> Address Book </div></div> <div class="mail-right-top"><p class="mail-head" align="left" >Search : <input type="text"style="width:88px;border:1px solid #449ed0;margin-top:4px;"></p> </div> <div class="mail-right-white1"><p>&nbsp;</p> <p class="normal-font"><a href="#">ar_thite</a></p> <p class="normal-font"><a href="#">sr_thite</a></p> <p class="normal-font"><a href="#">ar_thite</a></p> <p class="normal-font"><a href="#">ar_thite</a></p> <p class="normal-font"><a href="#">ar_thite</a></p> <p class="normal-font"><a href="#">ar_thite</a></p> <p>&nbsp;</p> </div> <div class="invitemenu"> <ul> <li><a href="#">Individual</a></li> <li><a href="#">Group</a></li> <li><a href="#">All</a></li> </ul> </div> </div> </td> <td width="2%"></td> </tr> </table> <p>&nbsp;</p> <p>&nbsp;</p> <table style="width:100%"><tr><td width="2%"></td><td><textarea id="mailCompose" style="height:300px;width:96%"></textarea></td><td width="2%"></td><tr></table> </div> <p>&nbsp;</p> <p style="float:right;"> <button class="mail-button-white"><a href="#">Send</a></button> <button class="mail-button-white"><a href="#">Save</a></button> <button class="mail-button-white" style="margin-right:10px;"><a href="#">Cancel</a></button></p><p>&nbsp;</p><p>&nbsp;</p> </div></div></div></div></div>';
	addEditorDefault('mailCompose');
}

var currentLink = 'null';
function toggleLinkBehaviour(obj,flag,curr)
{
	
	if(obj.innerHTML.indexOf(currentLink)==-1)
	{
		if(!flag)
		{
			obj.style.background='#449ed0';
			obj.style.color='white';
		}
		else
		{
			obj.style.background='#eaeaea';
			obj.style.color='black';
		}
		if(curr!=null)
		{
			$('change-area').innerHTML = '';
			currentLink = curr;
			releaseLinks();
		}
		//if(curr!=null)
		//	alert(curr+currentLink);
	}
}

function releaseLinks()
{
	var alinks = document.getElementsByTagName('a');
	for(var i=0;i<alinks.length;i++)
		if(alinks[i].className=='toggLinks')
			toggleLinkBehaviour(alinks[i],false,null);
}
function sippShow(action)
{
	var sipp = $('sippDesc');
	if(action=='soci')
		sipp.innerHTML = '<center><p class="sipp-head">Enjoy Different social Networking Features </br>With three different version</p></center><center><img src="latest/social3.png"/></center>';
	else if(action=='info')
		sipp.innerHTML = '<center><p class="sipp-head">Experience and Enjoy Social Networking </br>With three different version</p></center><center><img src="latest/info1.png"/></center>';
	else if(action=='prof')
		sipp.innerHTML = '<center><p class="sipp-head">Search Jobs and Career For your growth. </center><center><img src="latest/professional.png"/></center>';
	else if(action=='prom')
		sipp.innerHTML = '<center><p class="sipp-head">Promote Your Company And  </br>Product on Largest Stage</p></center><center><img src="latest/promoting.png"/></center>';
}
function sippPaint(action)
{
	if(action=='soci')
		initSocial();
}
function initSocial()
{
	if($('leftNavHolder')==null)
		prepTwoColumLayout(true);
	$('top-tabs').innerHTML = '<li style="margin-left: 1px"><a href="#" onclick="initSocial()" title="Home">Home</a></li> <li style="margin-left: 1px"><a href="#" title="Video" onclick="paintThemes()">Themes</a></li> <li><a href="#" title="SMS">SMS</a></li> <li style="margin-left: 1px"><a href="#" title="Chatting" onclick="paintChat()">Chatting</a></li> <li style="margin-left: 1px"><a href="#" title="Greeting" onclick="paintGreeting()">Greeting</a></li> <li style="margin-left: 1px"><a href="#" title="Invite" onclick="paintInvite()">Invite</a></li>';
	$('leftNavHolder').innerHTML = '<div class="left-cont"> <div class="midbl-top"><div class="midbr-top"><div class="midtl-top"><div class="midtr-top">	<center><img src="img/photo.gif"/ style="margin-top:10px;border:1px solid black;"></br> <b style="color:#449ed0;text-decoration:underline;">Amit Thite</b></br> </center> <div class="bod-left"> </div> <div class="left-links"> <ul> <li><a href="#" onclick="paintSocial(\'Scraps\')">Scrap</a></li> <li><a href="#" onclick="paintProfile(\'Self\')">Profile</a></li> <li><a href="#" onclick="getData(\'Friends\')">Friends </a></li> <li><a href="#" onclick="paintCommunities(\'Create\')">Groups</a></li> <li><a href="#" onclick="paintSocial(\'Autobio\')">Biography</a></li> <li><a href="#">Contacts</a></li> </ul> <p>&nbsp;</p> </div> </div></div></div></div> </div> <p>&nbsp;</p><div class="left-head"> <div class="midtl-top"><div class="midtr-top"> <center>Updates</center> </div></div> </div> <div class="left-cont"> <div class="midbl-top"><div class="midbr-top"> <div class="left-links"> <ul> <li><a href="#" onclick="paintMyList()">My List</a></li> <li><a href="#" onclick="paintHotLinks()">Hot Links</a></li> <li><a href="#" onclick="paintGallery(\'Create\')">Gallery</a></li> <li><a href="#" onclick="paintMessages(\'Inbox\')">Messages</a></li> <li><a href="#">Visitors List</a></li> <li><a href="#">Jobs Alert</a></li> <li><a href="#">Events</a></li> </ul> <p>&nbsp;</p> </div> </div></div> </div> <p>&nbsp;</p> <div class="left-head"> <div class="midtl-top"><div class="midtr-top"> <center>Daily Polls</center> </div></div> </div> <div class="left-cont"> <div class="midbl-top"><div class="midbr-top"> <p>&nbsp;</p> <p>&nbsp;</p> <p>&nbsp;</p> <p>&nbsp;</p> <p>&nbsp;</p> </div></div> </div>';
	var temp = '<div class="mid-cont"><div class="midtl-top"><div class="midtr-top"><p>&nbsp;</p><p>&nbsp;&nbsp;&nbsp;&nbsp;<font color="#ed8449"style="font-size:16px;font-weight:bold;" >Welcome</font> , <font color="#449ed0">Amit Thite</font>&nbsp;&nbsp;</p><p class="bod-left"></p> <p class="home-font">Last Login on 14 Apr 12.00 AM </p><p>&nbsp;</p><div class="home-links"><ul><li><a href="#" onclick="">Visitors</a></li><li><a href="#">Jobs Alert</a></li><li><a href="#">New Members</a></li><li><a href="#">Online</a></li> <li><a href="#">Bana Dost Friends</a></li></ul></div><p>&nbsp;</p>		<p>&nbsp;</p></div></div></div><p>&nbsp;</p><p>&nbsp;</p>';
	for(var i=0;i<5;i++)
		temp += '<div class="only-home1"> <div class="profile-head">&nbsp;&nbsp; Latest Visitors</div> <div class="midbl-top"><div class="midbr-top"><p>&nbsp;</p> <table><tr><td><center><img id="profile_image" src="img/photo.gif"style="border:1px solid black;margin:10px;"/><p>Amit Thite</p></center></td> <td><center><img id="profile_image" src="img/photo.gif"style="border:1px solid black;margin:10px;"/><p>Amit Thite</p></center></td><td><center><img id="profile_image" src="img/photo.gif"style="border:1px solid black;margin:10px;"/> <p>Amit Thite</p></center></td></tr></table><p>&nbsp;</p></div></div> </div><p>&nbsp;</p><p>&nbsp;</p>';
	$('rightContentHolder').innerHTML = temp;
}
function paintSocial(model)
{
	if($('leftNavHolder')==null)
		prepTwoColumLayout(true);
	$('rightContentHolder').innerHTML = '<div class="mid-cont-links"> <ul> <li><a id="write-link" href="#" class="toggLinks" style="background:#449ed0;" onmouseover="toggleLinkBehaviour(this,true,null);" onmouseout="toggleLinkBehaviour(this,false,null);" onclick="toggleLinkBehaviour(this,true,\'Write\');addNewEditor(\'change-area\',\'500px\',\'common-editor\');"><span>Write</span></a></li> <li><a class="toggLinks" href="#" style="background:#449ed0;" onmouseover="toggleLinkBehaviour(this,true,null);" onmouseout="toggleLinkBehaviour(this,false,null);" onclick="toggleLinkBehaviour(this,true,\'Edit\');"><span>Edit</span></a></li> <li><a class="toggLinks" href="#" style="background:#449ed0;" onmouseover="toggleLinkBehaviour(this,true,null);" onmouseout="toggleLinkBehaviour(this,false,null);" onclick="toggleLinkBehaviour(this,true,\'Read\');getData(\''+model+'\')"><span>Read</span></a></li> <li><a class="toggLinks" href="#" style="background:#449ed0;" onmouseover="toggleLinkBehaviour(this,true,null);" onmouseout="toggleLinkBehaviour(this,false,null);" onclick="toggleLinkBehaviour(this,true,\'Delete\');"><span>Delete</span></a></li> </ul> </div> <div class="mid-cont"> <div class="midbl-top"><div class="midbr-top"> <p>&nbsp;</p> <div style="width:100%" id="change-area"></div><p>&nbsp;</p><p>&nbsp;</p><p>&nbsp;</p></div></div></div>';
	toggleLinkBehaviour($('write-link'),true,'Write');
	addNewEditor('change-area','500px','common-editor');
}
function paintThemes()
{
	$('rightContentHolder').innerHTML = '<div class="box-head"> &nbsp;&nbsp;Themes :- Current Themes Name comes Here.. </div> <div class="mid-cont"> <div class="midbl-top"><div class="midbr-top"> <p>&nbsp;</p> <center> <table width="100%"><tr> <td width="20%"> <center> <div class="theme-name">Spring</div> <img id="profile_image" src="img/photo.gif"style="border:1px solid black;"/> <p><button style="background:#449ed0;color:white;padding:2px;">Apply</button></p></center> </td> <td> <center> <div class="theme-name">Spring</div> <img id="profile_image" src="img/photo.gif"style="border:1px solid black;"/> <p><button style="background:#449ed0;color:white;padding:2px;">Apply</button></p></center> </td> <td> <center> <div class="theme-name">Spring</div> <img id="profile_image" src="img/photo.gif"style="border:1px solid black;"/> <p><button style="background:#449ed0;color:white;padding:2px;">Apply</button></p></center> </td> <td> <center> <div class="theme-name">Spring</div> <img id="profile_image" src="img/photo.gif"style="border:1px solid black;"/> <p><button style="background:#449ed0;color:white;padding:2px;">Apply</button></p></center> </td> <td> <center> <div class="theme-name">Spring</div> <img id="profile_image" src="img/photo.gif"style="border:1px solid black;"/> <p><button style="background:#449ed0;color:white;padding:2px;">Apply</button></p></center> </td> </tr></table> </center> <p>&nbsp;</p> </div> </div> </div> </div> </div> </td> </td> <td width="2%"></td> </tr></table> <p>&nbsp;</p> <p>&nbsp;</p> <p>&nbsp;</p> <p>&nbsp;</p> </div> </div> </div> </div>';
}

function paintGreeting()
{
	$('rightContentHolder').innerHTML = '<div class="listmenu"> <ul> <li><a href="#"><span>Greetings</span></a></li> </ul> </div> <div class="list-box"> <div class="top-box"> <p>&nbsp;</p> <b style="margin-left:3%;color:#449ed0;">Email id : </b> <input  type="text"  value="" maxlength="255" style="margin-top:5px;width: 150px;border: 1px solid #449ed0;"/> <input type="button" value="Send" style="background:#449ed0;padding:2px;color:white;"> <b style="margin-left:3%;color:#449ed0;">Category : </b> <SELECT  style="width:150px;border: 1px solid #449ed0;"> <OPTION VALUE=""/>  Category <OPTION VALUE="1"/>  Birthday <OPTION VALUE="2"/>  Diwali <OPTION VALUE="3"/>  Holi <OPTION VALUE="4"/>  Christmas <OPTION VALUE="5"/>  Makar Sankrant <OPTION VALUE="6"/>  Independence Day	<OPTION VALUE="7"/>  Republic Day <OPTION VALUE="8"/>  Ganesh Chathurthi <OPTION VALUE="9"/>  Gokul Ashtami <OPTION VALUE="10"/> Raksha Bandhan <OPTION VALUE="11"/> Gudi Padwa <OPTION VALUE="12"/> Id <OPTION VALUE="14"/> Moharam <OPTION VALUE="15"/> Invitation <OPTION VALUE="16"/> Seasonal </SELECT>&nbsp;&nbsp; <p>&nbsp;</p> </div> <center> <table width="94%" style="margin:3%;"> <tr> <td width="1%"></td> <td width="18%"> <img src="img/photo.gif"style="border:1px solid black"/></br> <span><center><input type="checkbox">Select</input> </center></span> </td> <td width="1%"></td> <td width="18%"> <img   src="img/photo.gif"style="border:1px solid black"/> </br> <span><center><input type="checkbox">Select</input></center></span> </td> <td width="1%"></td> <td width="18%"> <img  src="img/photo.gif"style="border:1px solid black"/> </br> <span><center><input type="checkbox">Select</input></center></span> </td> <td width="1%"></td> <td width="18%"> <img  src="img/photo.gif"style="border:1px solid black"/> </br> <span><center><input type="checkbox">Select</input></center></span> </td> <td width="1%"></td> <td width="18%"> <img  src="img/photo.gif"style="border:1px solid black"/> </br> <span><center><input type="checkbox">Select</input></center></span> </td> <td width="1%"></td> </tr> </table> </center> <p>&nbsp;</p> </div> <p>&nbsp;</p>';
}

function paintInvite()
{
	$('rightContentHolder').innerHTML = '<div class="box"> <div class="box-head"> <center>Invite Your Friends And loving ones To Banadost</center> </div> <p>&nbsp;</p> <table style=" margin-left:9%;"> <tr> <td class="mess-head"> Subject:</td> <td ></td> <td> Amit invites you to Banadost </td> </tr> <tr><td>&nbsp;</td></tr> <tr> <td class="mess-head"> From:</td> <td></td> <td ID="senderEmailField" class="invitehomeRightColumn"> ar_thite@rediffmail.com </td> </tr> <tr><td>&nbsp;</td></tr> <tr> <td > <div class="mess-head"> To: </div> <div> (Separate multiple emails with comma)</div> </td> <td></td> <td> <textarea  rows="4" style="width: 240px;border:1px solid #cccccc;">enter email addresses</textarea></td> </tr> <tr> <td></td> <td></td> <td><input type = "checkbox" />Send your Photo With Invitation.</td></tr> </table> <p>&nbsp;</p> <center> <button style="background:#449ed0;color:white;padding:2px;"> Send </button></center> <p>&nbsp;</p> </div> <p>&nbsp;</p>';
}

function paintChat()
{
	$('rightContentHolder').innerHTML = '<div class="box-head"> <center>Chat With Your Friends and Family</center> </div> <div class="mid-cont"> <div class="midbl-top"><div class="midbr-top"> <div class="chat-box"> <table width="100%" valign="top"><tr> <td width="2%"></td> <td width="20%"><div class="white-back1"> <div class="graybl"><div class="graybr"><div class="graytl"><div class="graytr"> <center><img src="img/photo.gif"/ style="margin-top:10px;border:1px solid black;"></br> <b style="color:#449ed0;text-decoration:underline;">Sumit Thite</b></br> </center> <p>&nbsp;</p> <p> <b style="margin-left:3%;color:#449ed0;">Category:</b> <SELECT style="width:90px;margin-top:5px;border: 1px solid #eaeaea;"> <OPTION VALUE=""/>  Category <OPTION VALUE="1"/>  Friends <OPTION VALUE="2"/>  Group <OPTION VALUE="3"/>  Rooms </select> </p> <p>&nbsp;</p> <p> <b style="margin-left:3%;color:#449ed0;">Friends:</b> <SELECT style="width:90px;margin-top:5px;border: 1px solid #eaeaea;"> <OPTION VALUE=""/>  Friends <OPTION VALUE="1"/>  Amol <OPTION VALUE="2"/>  Sumit <OPTION VALUE="3"/>  Rohit </select> </p> <p>&nbsp;</p> <p>&nbsp;</p> </div> </div> </div> </div> </div> </td> <td width="2%"></td> <td width="74%"> <div class="white-back1"> <div class="graybl"><div class="graybr"><div class="graytl"><div class="graytr"> Message comes here <p>&nbsp;</p> <p>&nbsp;</p> <p>&nbsp;</p> <p>&nbsp;</p> <p>&nbsp;</p> </div> </div> </div> </div> </div> <div class="white-back1"> <div class="graybl"><div class="graybr"><div class="graytl"><div class="graytr"> You can type here.ssssssssssssssssssssss <p>&nbsp;</p> <p>&nbsp;</p> <p>&nbsp;</p> <p>&nbsp;</p> <p>&nbsp;</p> <button style="background:#449ed0;color:white;padding:2px;float:right;">Send</button> <p>&nbsp;</p> </div> </div> </div> </div> </div> </td> </td> <td width="2%"></td> </tr></table> <p>&nbsp;</p> <p>&nbsp;</p> <p>&nbsp;</p> <p>&nbsp;</p> </div> </div> </div> </div>';
}
function paintCommunities(type)
{
	$('rightContentHolder').innerHTML = '<div class="mid-cont-links"> <ul> <li><a id="write-link" href="#" class="toggLinks" style="background:#449ed0;" onmouseover="toggleLinkBehaviour(this,true,null);" onmouseout="toggleLinkBehaviour(this,false,null);" onclick="toggleLinkBehaviour(this,true,\'Create\');paintCommunities(\'Create\')"><span>Create</span></a></li> <li><a class="toggLinks" href="#" style="background:#449ed0;" onmouseover="toggleLinkBehaviour(this,true,null);" onmouseout="toggleLinkBehaviour(this,false,null);" onclick="toggleLinkBehaviour(this,true,\'Search\');paintCommunities(\'Search\')"><span>Search</span></a></li> <li><a class="toggLinks" href="#" style="background:#449ed0;" onmouseover="toggleLinkBehaviour(this,true,null);" onmouseout="toggleLinkBehaviour(this,false,null);" onclick="toggleLinkBehaviour(this,true,\'My Communities\');paintCommunities(\'All\')"><span>My Communities</span></a></li></ul></div><div class="mid-cont"><div class="midbl-top"><div class="midbr-top"><p>&nbsp;</p><div style="width:100%" id="change-area"></div><p>&nbsp;</p><p>&nbsp;</p><p>&nbsp;</p></div></div></div>';
	if(type=='Create')
		$('change-area').innerHTML = '<div class="mid-head"> <a href="#">Group :-</a> Create </div> <p>&nbsp;</p> <b style="color:#449ed0;text-decoration:underline;margin-left:5%;">Fullfill the information to create a Community</b>	<p>&nbsp;</p> <table style="margin-left:5%;color:#449ed0;"><tr> <td><b>Name: </b></td> <td><input id="fname" type="text" name="data[User][fname]" value="" maxlength="255" style="margin-top:5px;width: 150px;border: 1px solid #cccccc;"/><br/><br/></td></tr> <tr><td><b>Categoty: </b></td> <td><SELECT style="width:150px;margin-top:5px;border: 1px solid #cccccc;"> <OPTION VALUE=""/>  Categories <OPTION VALUE="1"/>  Agriculture <OPTION VALUE="2"/>  Architecture & Interior Designing <OPTION VALUE="3"/>  Arts & Entertainment <OPTION VALUE="4"/>  Business <OPTION VALUE="5"/>  Bikers <OPTION VALUE="6"/>  Cities <OPTION VALUE="7"/>  Company <OPTION VALUE="8"/>  Computers & Internet <OPTION VALUE="9"/>  Countries <OPTION VALUE="10"/> Colloges & Universities <OPTION VALUE="11"/> Cultures <OPTION VALUE="12"/> Family & Home <OPTION VALUE="13"/>  Fashion & Beauty <OPTION VALUE="14"/>  Forum <OPTION VALUE="15"/>  Games <OPTION VALUE="16"/>  Government & Politics <OPTION VALUE="17"/>  Health & Fitness <OPTION VALUE="18"/>  History <OPTION VALUE="19"/>  Music <OPTION VALUE="20"/>  Nature <OPTION VALUE="21"/>  Recreation <OPTION VALUE="22"/>  Religion <OPTION VALUE="23"/>  Romance <OPTION VALUE="24"/>  Schools & Education <OPTION VALUE="25"/>  Science & Technology <OPTION VALUE="26"/>  Sports <OPTION VALUE="27"/>  Spiritual <OPTION VALUE="28"/>  State & Proveince <OPTION VALUE="29"/>  Travel <OPTION VALUE="30"/>  Other </select></br></br></td></tr> <tr><td><b>Description: </b></td> <td><textarea name="body" cols="40" rows="3" style="border:1px solid #cccccc; "></textarea> <br/><br/></td></tr> <tr><td><b>Type: </b></td> <td><SELECT style="width:150px;margin-top:5px;border: 1px solid #cccccc;"> <OPTION VALUE="1"/>  Private <OPTION VALUE="2"/>  Public </select></br></br></td></tr> <tr><td> <b style="margin-left:5%;color:#449ed0;"> Image: </b></td> <td><input type="file" name="photo" accept="image/*" size="16" /></td></tr></table> <p>&nbsp;</p> <center><button style="background:#449ed0;color:white;padding:2px;">Create</button></center> <p>&nbsp;</p> <p>&nbsp;</p>';
	else if(type=='Search')
		$('change-area').innerHTML = '<div class="mid-head"> <a href="#">Groups :-</a> Search </div> <p>&nbsp;</p> <b style="color:#449ed0;text-decoration:underline;margin-left:5%;">You Can Search Communities by category</b> <p>&nbsp;</p> <table style="margin-left:5%;"> <tr> <td> <b style="color:#449ed0;">Search:</b> <input type="text"  value="" maxlength="355" style="margin-top:5px;width: 150px;border: 1px solid #cccccc;"/> </td> <td>&nbsp;&nbsp;&nbsp; </td> <td> <SELECT style="width:150px;margin-top:5px;border: 1px solid #cccccc;"> <OPTION VALUE=""/>  Categories <OPTION VALUE="1"/>  Agriculture <OPTION VALUE="2"/>  Architecture & Interior Designing <OPTION VALUE="3"/>  Arts & Entertainment <OPTION VALUE="4"/>  Business <OPTION VALUE="5"/>  Bikers <OPTION VALUE="6"/>  Cities <OPTION VALUE="7"/>  Company <OPTION VALUE="8"/>  Computers & Internet <OPTION VALUE="9"/>  Countries <OPTION VALUE="10"/> Colloges & Universities <OPTION VALUE="11"/> Cultures <OPTION VALUE="12"/> Family & Home <OPTION VALUE="13"/>  Fashion & Beauty <OPTION VALUE="14"/>  Forum <OPTION VALUE="15"/>  Games <OPTION VALUE="16"/>  Government & Politics <OPTION VALUE="17"/>  Health & Fitness <OPTION VALUE="18"/>  History <OPTION VALUE="19"/>  Music <OPTION VALUE="20"/>  Nature <OPTION VALUE="21"/>  Recreation <OPTION VALUE="22"/>  Religion <OPTION VALUE="23"/>  Romance <OPTION VALUE="24"/>  Schools & Education <OPTION VALUE="25"/>  Science & Technology <OPTION VALUE="26"/>  Sports <OPTION VALUE="27"/>  Spiritual <OPTION VALUE="28"/>  State & Proveince <OPTION VALUE="29"/>  Travel <OPTION VALUE="30"/>  Other </SELECT> </td> </tr> </table> <p>&nbsp;</p> <center><input value="&nbsp;&nbsp;Search&nbsp;&nbsp;" type="submit" style="background:#449ed0;color:white;padding:2px;"/></center> <p>&nbsp;</p> <p>&nbsp;</p> <center> <div class="friends-head"> <div class="mid-conttl"><div class="mid-conttr"> <p>&nbsp;</p> </div></div> </div> <div class="groups"> <div class="mid-contbl"><div class="mid-contbr"> <table width="100%" style="margin-left:10px;"><tr> <td width="15%" style="border:1px solid black;"> <img src="img/photo.gif"/ style="margin-top:3px;border:1px solid black;"></br> <b style="color:#449ed0;">Banadost</b></br> </td> <td width="75%" align="left" valign="top"> <p class="updates-info">Group description comes here</br> Group description comes here</br> Group description comes here</br> <p>&nbsp;</p> <center><button style="background:#449ed0;color:white;padding:2px;" onclick="paintCommunities(\'\')">Veiw </button></center> </p> </td> </tr> </table> <p>&nbsp;</p> </div></div> </div> </center> <p>&nbsp;</p>';
	else if(type=='All')
		$('change-area').innerHTML = '<div class="mid-head"> <a href="#">Groups :-</a> My Groups </div> <p>&nbsp;</p> <center> <div class="friends-head"> <div class="mid-conttl"><div class="mid-conttr"> <p>&nbsp;</p> </div></div> </div> <div class="groups"> <div class="mid-contbl"><div class="mid-contbr"> <table width="100%" style="margin-left:10px;"><tr> <td width="15%" style="border:1px solid black;"> <img src="img/photo.gif"/ style="margin-top:3px;border:1px solid black;"></br> <b style="color:#449ed0;">Banadost</b></br> </td> <td width="75%" align="left" valign="top"> <p class="updates-info">Group description comes here</br> Group description comes here</br> Group description comes here</br> <p>&nbsp;</p> <center><button style="background:#449ed0;color:white;padding:2px;" onclick="paintCommunities(\'\')">Veiw </button></center> </p> </td> </tr> </table> <p>&nbsp;</p> </div></div> </div> </center> <p>&nbsp;</p> <p>&nbsp;</p> <p>&nbsp;</p>';
	else
		$('change-area').innerHTML = '<p style="background:url(latest/blu1-top.png) repeat-x; width:90%;margin-left:5%;"><b style="margin-left:2%;color:white;">Group Name :- Banadost</b></p> <table style="margin-left:7%;"><tr><td><img src="img/photo.gif"style="margin-left:10px;border:1px solid black"/></td> <td> </br> <b>Owner Name:</b>&nbsp;Sumeet</br><b>Category:</b>&nbsp;Art & Entertainment</br><b>Created on:</b>&nbsp;21/1/2009</br> </br> </br> </td></tr></table> <p style="background:url(latest/blu1-top.png) repeat-x; width:90%;margin-left:5%;"><b style="margin-left:2%;color:white;">Description:</b></p> <p style=" width:90%;margin-left:5%;margin-top:1%;color:#449ed0;">baap of all social networking site and its goinging to be launched 15th may 2009 ..Its going to have a grand opening vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv</p> <p>&nbsp;</p> <center><button style="background:#449ed0;color:white;padding:2px;">Join</button></center></center> <p>&nbsp;</p> <p>&nbsp;</p> <p style="background:url(latest/blu1-top.png) repeat-x; width:90%;margin-left:5%;"><b style="margin-left:2%;color:white;"> Members :- Banadost</b></p> <center> <table width="90%" style="margin:3%;"> <tr width="90%"> <td width="18%"> <img title="member"src="img/photo.gif"style="border:1px solid black"/></br> <span><center>Amit</center></span> </td> <td width="1%"></td> <td width="18%"> <img  title="member" src="img/photo.gif"style="border:1px solid black"/> </br> <span><center>Amit</center></span> </td> <td width="1%"></td> <td width="18%"> <img title="member" src="img/photo.gif"style="border:1px solid black"/> </br> <span><center>Amit</center></span> </td> <td width="1%"></td> <td width="18%"> <img title="member" src="img/photo.gif"style="border:1px solid black"/> </br> <span><center>Amit</center></span> </td> <td width="1%"></td> <td width="18%"> <img title="member" src="img/photo.gif"style="border:1px solid black"/> </br> <span><center>Amit</center></span> </td> </tr> <tr> <td width="18%"> <img title="member"src="img/photo.gif"style="border:1px solid black"/></br> <span><center>Amit</center></span> </td> <td width="1%"></td> <td width="18%"> <img  title="member" src="img/photo.gif"style="border:1px solid black"/> </br> <span><center>Amit</center></span> </td> <td width="1%"></td> <td width="18%"> <img title="member" src="img/photo.gif"style="border:1px solid black"/> </br> <span><center>Amit</center></span> </td> <td width="1%"></td> <td width="18%"> <img title="member" src="img/photo.gif"style="border:1px solid black"/> </br> <span><center>Amit</center></span> </td> <td width="1%"></td> <td width="18%"> <img title="member" src="img/photo.gif"style="border:1px solid black"/></br> <span><center>Amit</center></span> </td> </tr> <tr> <td width="18%"> <img title="member"src="img/photo.gif"style="border:1px solid black"/></br> <span><center>Amit</center></span> </td> <td width="1%"></td> <td width="18%"> <img  title="member" src="img/photo.gif"style="border:1px solid black"/></br> <span><center>Amit</center></span> </td> <td width="1%"></td> <td width="18%"> <img title="member" src="img/photo.gif"style="border:1px solid black"/></br> <span><center>Amit</center></span> </td> <td width="1%"></td> <td width="18%"> <img title="member" src="img/photo.gif"style="border:1px solid black"/></br> <span><center>Amit</center></span> </td> <td width="1%"></td> <td width="18%"> <img title="member" src="img/photo.gif"style="border:1px solid black"/></br> <span><center>Amit</center></span> </td> </tr> </table> </center> <p>&nbsp;</p> <p>&nbsp;</p>';
}
function paintProfile(which)
{
	if(which=='Self')
	{
		var temp = '';
		temp = ' <table width="100%" valign="top"><tr><td width="75%"><div class="profile-box"> <div class="profile-head">&nbsp;Profile :- <b style="color:black;font-size:13px;">General <a href="#" style="float:right;;color:black;margin:2px;">Edit</a></b> </div> <form> <table width="100%" style="margin-left:5%;"> <tr> <td><b style="margin-left:3%;color:#449ed0;">First Name: </b> </td> <td><input  type="text" name="data[User][fname]" value="" maxlength="255" style="margin-top:5px;width: 150px;border: 1px solid #eaeaea;"/><br/></td> </tr> <tr><td>&nbsp;</td> </tr> <tr> <td><b style="margin-left:3%;color:#449ed0;">Last Name:</b> </td> <td><input    value="" maxlength="255" style="margin-top:5px;width: 150px;border: 1px solid #eaeaea;"/><br/></td></tr> <tr><td>&nbsp;</td> </tr> <tr> <td><b style="margin-left:3%;color:#449ed0;">Gender:</b></td> <td>Male<input name="gender"  type="radio" value="m" class="leftal" /> Female<input name="gender" id="addfemale" type="radio" value="f"  /></br></td></tr> <tr><td>&nbsp;</td> </tr> <tr> <td><b style="margin-left:3%;color:#449ed0;">Date of Birth:</b></td> <td>'+getDateSelect(1900)+'&nbsp;&nbsp; </br></td></tr> <tr><td>&nbsp;</td> </tr> <tr> <td><b style="margin-left:3%;color:#449ed0;">Zodiac Sign:</b></td> <td><SELECT id="sec_qs" NAME="data[User][security_question]" style="width:80px;margin-top:5px;border: 1px solid #eaeaea;"> <OPTION VALUE=""/>  -Sign- <OPTION VALUE="1"/>  Aries <OPTION VALUE="2"/>  Taurus <OPTION VALUE="3"/>  Gemini <OPTION VALUE="4"/>  Cancer <OPTION VALUE="5"/>  Leo <OPTION VALUE="6"/>  Virgo <OPTION VALUE="7"/>  Libra <OPTION VALUE="8"/>  Scorpio <OPTION VALUE="9"/>  Scorpio <OPTION VALUE="10"/> Capricorn <OPTION VALUE="11"/> Aquarius <OPTION VALUE="12"/> Pisces </SELECT> </br></td></tr> <tr><td>&nbsp;</td> </tr> <tr> <td><b style="margin-left:3%;color:#449ed0;">City:</b></td>  <td><input  type="text" name="data[User][fname]" value="" maxlength="255" style="margin-top:5px;width: 150px;border: 1px solid #eaeaea;"/><br/></td></tr> <tr><td>&nbsp;</td> </tr> <tr><td> <b style="margin-left:3%;color:#449ed0;">State:</b></td> <td><input  type="text" name="data[User][fname]" value="" maxlength="255" style="margin-top:5px;width: 150px;border: 1px solid #eaeaea;"/><br/></td> </tr> <tr><td>&nbsp;</td> </tr> <tr> <td> <b style="margin-left:3%;color:#449ed0;">Country:</b></td> <td><SELECT  style="width:150px;margin-top:5px;border: 1px solid #eaeaea;">  <OPTION VALUE="">  ------Select------- <OPTION VALUE="1">  AFGHANISTAN <OPTION VALUE="2">  ALBANIA <OPTION VALUE="3">  ALGERIA <OPTION VALUE="4">  AMERICAN <OPTION VALUE="5">  SAMOA <OPTION VALUE="6">  ANDORRA <OPTION VALUE="7">  ANGOLA <OPTION VALUE="8">  ANGUILLA <OPTION VALUE="9">  ANTARCTICA <OPTION VALUE="10">  ANTIGUA <OPTION VALUE="11">  AND <OPTION VALUE="12">  BARBUDA <OPTION VALUE="13">  ARGENTINA <OPTION VALUE="14">  ARMENIA <OPTION VALUE="15">  ARUBA <OPTION VALUE="16">  AUSTRALIA <OPTION VALUE="17">  AUSTRIA <OPTION VALUE="18">  AZERBAIJAN <OPTION VALUE="19">  BAHAMAS <OPTION VALUE="20">  BAHRAIN <OPTION VALUE="21">  BANGLADESH <OPTION VALUE="22">  BARBADOS <OPTION VALUE="23">  BELARUS <OPTION VALUE="24">   BELGIUM <OPTION VALUE="25">   BELIZE <OPTION VALUE="25">   BENIN <OPTION VALUE="27">   BERMUDA <OPTION VALUE="28">   BHUTAN <OPTION VALUE="29">   BOLIVIA <OPTION VALUE="30">  BOSNIA AND HERZEGOVINA <OPTION VALUE="31">  BOTSWANA <OPTION VALUE="32">  BOUVET ISLAND <OPTION VALUE="33">  BRAZIL <OPTION VALUE="34">  BRITISH INDIAN OCEAN TERRITORY <OPTION VALUE="35">  BRUNEI <OPTION VALUE="36">  DARUSSALAM <OPTION VALUE="37">  BULGARIA <OPTION VALUE="38">  BURKINA <OPTION VALUE="39">  FASO <OPTION VALUE="40">  BURUNDI <OPTION VALUE="41">  CAMBODIA <OPTION VALUE="42">  CAMEROON <OPTION VALUE="43">  CANADA <OPTION VALUE="44">  CAPE VERDE <OPTION VALUE="45">  CAYMAN ISLANDS <OPTION VALUE="46">  CENTRAL AFRICAN REPUBLIC <OPTION VALUE="47">  CHAD <OPTION VALUE="48">  CHILE <OPTION VALUE="49">  CHINA <OPTION VALUE="50">  CHRISTMAS ISLAND <OPTION VALUE="51">  COCOS (KEELING) ISLANDS <OPTION VALUE="52">  COLOMBIA <OPTION VALUE="53">  COMOROS <OPTION VALUE="54">  CONGO <OPTION VALUE="55">  CONGO (THE DEMOCRATIC REPUBLIC) <OPTION VALUE="56">  COOK ISLANDS <OPTION VALUE="57">  COSTA RICA <OPTION VALUE="58">  COTE DIVOIRE <OPTION VALUE="59">  CROATIA <OPTION VALUE="60">  CUBA <OPTION VALUE="61">  CYPRUS <OPTION VALUE="62">  CZECH REPUBLIC <OPTION VALUE="63">  DENMARK <OPTION VALUE="64">  DJIBOUTI <OPTION VALUE="65">  DOMINICA <OPTION VALUE="66">  DOMINICAN REPUBLIC <OPTION VALUE="67">  ECUADOR <OPTION VALUE="68">  EGYPT <OPTION VALUE="69">  EL SALVADOR <OPTION VALUE="70">  EQUATORIAL GUINEA <OPTION VALUE="71">  ERITREA <OPTION VALUE="72">  ESTONIA <OPTION VALUE="73">  ETHIOPIA <OPTION VALUE="74">  FALKLAND ISLANDS (MALVINAS) <OPTION VALUE="75">  FAROE ISLANDS <OPTION VALUE="76">  FIJI <OPTION VALUE="77">  FINLAND <OPTION VALUE="78">  FRANCE <OPTION VALUE="79">  FRENCH GUIANA <OPTION VALUE="80">  FRENCH POLYNESIA <OPTION VALUE="81">  FRENCH SOUTHERN TERRITORIES <OPTION VALUE="82">  GABON <OPTION VALUE="83">  GAMBIA <OPTION VALUE="84">  GEORGIA <OPTION VALUE="85">  GERMANY <OPTION VALUE="86">  GHANA <OPTION VALUE="87">  GIBRALTAR <OPTION VALUE="88">  GREECE <OPTION VALUE="89">  GREENLAND <OPTION VALUE="90">  GRENADA <OPTION VALUE="91">  GUADELOUPE <OPTION VALUE="92">  GUAM <OPTION VALUE="93">  GUATEMALA <OPTION VALUE="94">  GUINEA <OPTION VALUE="95">  GUINEA-BISSAU <OPTION VALUE="96">  GUYANA <OPTION VALUE="93">  HAITI <OPTION VALUE="94">  HEARD ISLAND AND MCDONALD ISLANDS <OPTION VALUE="95">  HOLY SEE (VATICAN CITY STATE) <OPTION VALUE="97">  HONDURAS <OPTION VALUE="98">  HONG <OPTION VALUE="99">  KONG <OPTION VALUE="100">  HUNGARY <OPTION VALUE="100">  ICELAND <OPTION VALUE="100">  INDIA <OPTION VALUE="100">  INDONESIA <OPTION VALUE="100">  IRAN <OPTION VALUE="100">  IRAQ <OPTION VALUE="100">  IRELAND <OPTION VALUE="100">  ISRAEL <OPTION VALUE="100">  ITALY <OPTION VALUE="100">  JAMAICA <OPTION VALUE="100">  JAPAN <OPTION VALUE="100">  JORDAN <OPTION VALUE="100">  KAZAKHSTAN <OPTION VALUE="100">  KENYA <OPTION VALUE="100">  KIRIBATI <OPTION VALUE="100">  KOREA <OPTION VALUE="100">  KOREA <OPTION VALUE="100">  KUWAIT <OPTION VALUE="100">  KYRGYZSTAN <OPTION VALUE="100">  LAO PEOPLES DEMOCRATIC REPUBLIC <OPTION VALUE="100">  LATVIA <OPTION VALUE="100">  LEBANON <OPTION VALUE="100">  LESOTHO <OPTION VALUE="100">  LIBERIA <OPTION VALUE="100">  LIBYAN ARAB JAMAHIRIYA <OPTION VALUE="100">  LIECHTENSTEIN <OPTION VALUE="100">  LITHUANIA <OPTION VALUE="100">  LUXEMBOURG <OPTION VALUE="100">  MACAO <OPTION VALUE="100">  MACEDONIA <OPTION VALUE="100">  MADAGASCAR <OPTION VALUE="100">  MALAWI <OPTION VALUE="100">  MALAYSIA <OPTION VALUE="100">  MALDIVES <OPTION VALUE="100">  MALI <OPTION VALUE="100">  MALTA <OPTION VALUE="100">  MARSHALL ISLANDS <OPTION VALUE="100">  MARTINIQUE <OPTION VALUE="100">  MAURITANIA <OPTION VALUE="100">  MAURITIUS <OPTION VALUE="100">  MAYOTTE <OPTION VALUE="100">  MEXICO <OPTION VALUE="100">  MICRONESIA <OPTION VALUE="100">  MOLDOVA <OPTION VALUE="100">  MONACO <OPTION VALUE="100">  MONGOLIA <OPTION VALUE="100">  MONTSERRAT <OPTION VALUE="100">  MOROCCO <OPTION VALUE="100">  MOZAMBIQUE <OPTION VALUE="100">  MYANMAR <OPTION VALUE="100">  NAMIBIA <OPTION VALUE="100">  NAURU <OPTION VALUE="100">  NEPAL <OPTION VALUE="100">  NETHERLANDS <OPTION VALUE="100">  NETHERLANDS ANTILLES <OPTION VALUE="100">  NEW CALEDONIA <OPTION VALUE="100">  NEW ZEALAND <OPTION VALUE="100">  NICARAGUA <OPTION VALUE="100">  NIGER <OPTION VALUE="100">  NIGERIA <OPTION VALUE="100">  NIUE <OPTION VALUE="100">  NORFOLK <OPTION VALUE="100">  ISLAND <OPTION VALUE="100">  NORTHERN <OPTION VALUE="100">  MARIANA <OPTION VALUE="100">  ISLANDS <OPTION VALUE="100">  NORWAY <OPTION VALUE="100">  OMAN <OPTION VALUE="100">  PAKISTAN <OPTION VALUE="100">  PALAU <OPTION VALUE="100">  PALESTINIAN TERRITORY <OPTION VALUE="100">  PANAMA <OPTION VALUE="100">  PAPUA NEW GUINEA <OPTION VALUE="100">  PARAGUAY <OPTION VALUE="100">  PERU <OPTION VALUE="100">  PHILIPPINES <OPTION VALUE="100">  PITCAIRN <OPTION VALUE="100">  POLAND <OPTION VALUE="100">  PORTUGAL <OPTION VALUE="100">  PUERTO <OPTION VALUE="100">  RICO <OPTION VALUE="100">  QATAR <OPTION VALUE="100">  REUNION <OPTION VALUE="100">  ROMANIA <OPTION VALUE="100">  RUSSIAN <OPTION VALUE="100">  FEDERATION <OPTION VALUE="100">  RWANDA <OPTION VALUE="100">  SAINT <OPTION VALUE="100">  HELENA <OPTION VALUE="100">  SAINT KITTS AND NEVIS <OPTION VALUE="100">  SAINT LUCIA <OPTION VALUE="100">  SAINT PIERRE AND MIQUELON <OPTION VALUE="100">  SAINT VINCENT AND THE GRENADINES <OPTION VALUE="100">  SAMOA <OPTION VALUE="100">  SAN <OPTION VALUE="100">  MARINO <OPTION VALUE="100">  SAO <OPTION VALUE="100">  TOME <OPTION VALUE="100">  AND <OPTION VALUE="100">  PRINCIPE <OPTION VALUE="100">  SAUDI <OPTION VALUE="100">  ARABIA <OPTION VALUE="100">  SENEGAL <OPTION VALUE="100">  SERBIA AND MONTENEGRO <OPTION VALUE="100">  SEYCHELLES <OPTION VALUE="100">  SIERRA <OPTION VALUE="100">  LEONE <OPTION VALUE="100">  SINGAPORE <OPTION VALUE="100">  SLOVAKIA <OPTION VALUE="100">  SLOVENIA <OPTION VALUE="100">  SOLOMON <OPTION VALUE="100">  ISLANDS <OPTION VALUE="100">  SOMALIA <OPTION VALUE="100">  SOUTH <OPTION VALUE="100">  AFRICA <OPTION VALUE="100">  SOUTH GEORGIA & SOUTH SANDWICH ISLANDS <OPTION VALUE="100">  SPAIN <OPTION VALUE="100">  SRI LANKA <OPTION VALUE="100">  SUDAN <OPTION VALUE="100">  SURINAME <OPTION VALUE="100">  SVALBARD AND JAN MAYEN <OPTION VALUE="100">  SWAZILAND <OPTION VALUE="100">  SWEDEN <OPTION VALUE="100">  SWITZERLAND <OPTION VALUE="100">  SYRIAN <OPTION VALUE="100">  ARAB <OPTION VALUE="100">  REPUBLIC <OPTION VALUE="100">  TAIWAN <OPTION VALUE="100">  TAJIKISTAN <OPTION VALUE="100">  TANZANIA <OPTION VALUE="100">  THAILAND <OPTION VALUE="100">  TIMOR-LESTE <OPTION VALUE="100">  TOGO <OPTION VALUE="100">  TOKELAU <OPTION VALUE="100">  TONGA <OPTION VALUE="100">  TRINIDAD AND TOBAGO <OPTION VALUE="100">  TUNISIA <OPTION VALUE="100">  TURKEY <OPTION VALUE="100">  TURKMENISTAN <OPTION VALUE="100">  TURKS AND CAICOS ISLANDS <OPTION VALUE="100">  TUVALU <OPTION VALUE="100">  UGANDA <OPTION VALUE="100">  UKRAINE <OPTION VALUE="100">  UNITED ARAB EMIRATES <OPTION VALUE="100">  UNITED KINGDOM <OPTION VALUE="100">  UNITED STATES <OPTION VALUE="100">  UNITED STATES MINOR OUTLYING ISLANDS <OPTION VALUE="100">  URUGUAY <OPTION VALUE="100">  UZBEKISTAN <OPTION VALUE="100">  VANUATU <OPTION VALUE="100">  VENEZUELA <OPTION VALUE="100">  VIET NAM <OPTION VALUE="100">  VIRGIN ISLANDS (BRITISH) <OPTION VALUE="100">  VIRGIN ISLANDS (US) <OPTION VALUE="100">  WALLIS AND FUTUNA <OPTION VALUE="100">  WESTERN SAHARA <OPTION VALUE="100">  YEMEN <OPTION VALUE="100">  ZAMBIA <OPTION VALUE="100">  ZIMBABWE </SELECT><br/></td></tr></table> <p>&nbsp;</p> <center><input  class="bluebutt" value="&nbsp;&nbsp;Save&nbsp;&nbsp;" class="topformbutton" type="submit" style="background:#449ed0;color:white;padding:2px;"/></center> <p>&nbsp;</p> </form> </div> </td> <td width="25%"> <div class="profile-tabs"> <ul> <li><a href="#">General</a></li> <li><a href="#">Personal</a></li> <li><a href="#">Educational</a></li> <li><a href="#">Social</a></li> <ul> </div> <p>&nbsp;</p> <p>&nbsp;</p> <p>&nbsp;</p> <p>&nbsp;</p> <div class="comment-box"> <div class="box-head"><center>Rate Profile</center></div> <p>&nbsp;</p> <p>&nbsp;</p> <p>&nbsp;</p> <p>&nbsp;</p> <p>&nbsp;</p> <p>&nbsp;</p> </div> </td> </tr> </table> <p>&nbsp;</p>';
		$('rightContentHolder').innerHTML = temp;
	}
}
function paintMyList()
{
	$('rightContentHolder').innerHTML = '<div class="listmenu"> <ul> <li><a href="#"><span>My List</span></a></li> </ul> </div> <div class="list-box"> <div class="top-box"> <p>&nbsp;</p> <p><a href="#" class="mid-head"><img src="latest/smallfav.png"/>Favourite (12) </a><a href="#" class="mid-head"><img src="latest/smallblocked.png"/>Blocked (12) </a><a href="#" class="mid-head"><img src="latest/small-fan.png"/>Fans (12) </a></p> <p>&nbsp;</p> </div> <center> <table width="94%" style="margin:3%;"> <tr> <td width="1%"></td> <td width="18%"> <img src="img/photo.gif"style="border:1px solid black"/></br> <span><center><a href="#">Amit</a> </center></span> </td> <td width="1%"></td> <td width="18%"> <img   src="img/photo.gif"style="border:1px solid black"/> </br> <span><center><a href="#">Amit</a></center></span> </td> <td width="1%"></td> <td width="18%"> <img  src="img/photo.gif"style="border:1px solid black"/> </br> <span><center><a href="#">Amit</a></center></span> </td> <td width="1%"></td> <td width="18%"> <img  src="img/photo.gif"style="border:1px solid black"/> </br> <span><center><a href="#">Amit</a></center></span> </td> <td width="1%"></td> <td width="18%"> <img  src="img/photo.gif"style="border:1px solid black"/> </br> <span><center><a href="#">Amit</a></center></span> </td> <td width="1%"></td> </tr> </table> </center> <p>&nbsp;</p> </div> <p>&nbsp;</p>'
}
function paintHotLinks()
{
	$('rightContentHolder').innerHTML = '<div class="listmenu"> <ul> <li><a href="#"><span>Hot Links</span></a></li> </ul> </div> <div class="hotlink-box"> <div class="top-box"> <p>&nbsp;</p> <a href="#"><img src = "latest/small-new-group.png" title="Group" style="margin-left:20px;"/></a> <a href="#"><img src = "latest/small-new-vedio.png"title="Vedio" style="margin-left:20px;"/></a> <a href="#"><img src = "latest/small-new-music.png"title="Music" style="margin-left:20px;"/></a> <a href="#"><img src = "latest/small-new-gallery.png"title="Gallery" style="margin-left:20px;"/></a> <a href="#"><img src = "latest/small-new-profile.png"title="profile" style="margin-left:20px;"/></a> <a href="#"><img src = "latest/smallfriends.png"title="Friend" style="margin-left:20px;"/></a> <a href="#"><img src = "latest/smallscrap.png"title="Scrap" style="margin-left:20px;"/></a> <a href="#"><img src = "latest/smallmessage.png"title="Message" style="margin-left:20px;"/></a> <p>&nbsp;</p> </div> <center> <p class="gray-back"><b style="float:left;color:gray;">19/5/2009</b><a href="#">Description Comes here</a> <button style="color:white;padding:0px;background:#449ed0;float:right;">Delete</button> </p> <p class="gray-back"><b style="float:left;color:gray;">19/5/2009</b><a href="#">Description Comes here</a> <button style="color:white;padding:0px;background:#449ed0;float:right;">Delete</button> </p> <p class="gray-back"><b style="float:left;color:gray;">19/5/2009</b><a href="#">Description Comes here</a> <button style="color:white;padding:0px;background:#449ed0;float:right;">Delete</button> </p> </center> <p>&nbsp;</p> <p>&nbsp;</p> <p>&nbsp;</p> </div> <p>&nbsp;</p>';
}
function paintMessages(type)
{
	var temp = '<div class="invitemenu"> <ul> <li><a href="#" onclick="paintMessages(\'Inbox\')">Inbox</a></li> <li><a href="#" onclick="paintMessages(\'Sent\')">Sent</a></li> <li><a href="#" onclick="paintMessages(\'Compose\')">Compose</a></li> </ul> </div>';
	if(type=='Sent')
		temp += '<div class="invite-box"> <p>&nbsp;</p> <table width="100%"> <tr class="mess-head"> <td width="2%"></td> <td width="11%"> From </td> <td width="2%"></td> <td width="65%"> Subject </td> <td width="2%"></td> <td width="9%"> Date </td> <td width="2%"></td> <td width="5%"> </td> <td width="2%"></td> </tr> <tr><td>&nbsp;</td></tr> <tr> <td width="2%"></td> <td width="11%"> Sumeet Cheetri </td> <td width="2%"></td> <td width="65%"> <a href="#" style="color:black;" onclick="paintMessages(\'\')">How Are You Amit</a> </td> <td width="2%"></td> <td width="9%"> 19/5/2009 </td> <td width="2%"></td> <td width="5%"> <button style="background:#449ed0;color:white;padding:2px;">Delete</button> </td> <td width="2%"></td> </tr> <tr><td>&nbsp;</td></tr> <tr> <td width="2%"></td> <td width="11%"> Sumeet Cheetri </td> <td width="2%"></td> <td width="65%"> <a href="#" style="color:black;" onclick="paintMessages(\'\')">How Are You Amit</a> </td> <td width="2%"></td> <td width="9%"> 19/5/2009 </td> <td width="2%"></td> <td width="5%"> <button style="background:#449ed0;color:white;padding:2px;">Delete</button> </td> <td width="2%"></td> </tr> </table> </div> <p>&nbsp;</p> <p>&nbsp;</p> <p>&nbsp;</p>';
	else if(type=='Inbox')
		temp += '<div class="invite-box"> <p>&nbsp;</p> <table width="100%"> <tr class="mess-head"> <td width="2%"></td> <td width="11%"> To </td> <td width="2%"></td> <td width="65%"> Subject </td> <td width="2%"></td> <td width="9%"> Date </td> <td width="2%"></td> <td width="5%"> </td> <td width="2%"></td> </tr> <tr><td>&nbsp;</td></tr> <tr> <td width="2%"></td> <td width="11%"> Sumeet Cheetri </td> <td width="2%"></td> <td width="65%"> <a href="#" style="color:black;" onclick="paintMessages(\'\')">How Are You Amit</a> </td> <td width="2%"></td> <td width="9%"> 19/5/2009 </td> <td width="2%"></td> <td width="5%"> <button style="background:#449ed0;color:white;padding:2px;">Delete</button> </td> <td width="2%"></td> </tr> <tr><td>&nbsp;</td></tr> <tr> <td width="2%"></td> <td width="11%"> Sumeet Cheetri </td> <td width="2%"></td> <td width="65%"> <a href="#" style="color:black;" onclick="paintMessages(\'\')">How Are You Amit</a> </td> <td width="2%"></td> <td width="9%"> 19/5/2009 </td> <td width="2%"></td> <td width="5%"> <button style="background:#449ed0;color:white;padding:2px;">Delete</button> </td> <td width="2%"></td> </tr> </table> </div> <p>&nbsp;</p> <p>&nbsp;</p> <p>&nbsp;</p>';
	else if(type=='Compose')
		temp += '<div class="invite-box"> <p>&nbsp;</p> <table style="margin-left:30%;"> <tr align="left"> <td class="mid-head">To : </td> <td><input type="checkbox"/>&nbsp;Friends&nbsp;&nbsp;<input type="checkbox" />&nbsp;Group</td> </tr> <tr><td>&nbsp;</td></tr> <tr> <td class="mid-head">Friend List : </td> <td> <SELECT style="border: 1px solid #cccccc;"> <OPTION VALUE=""/>Friends <OPTION VALUE="1"/>aaasssssssss <OPTION VALUE="2"/>vvv <OPTION VALUE="3"/>ddd </SELECT> </td> </tr> <tr><td>&nbsp;</td></tr> <tr> <td class="mid-head">Subject : </td> <td> <input type="text" style="width:200px; border: 1px solid #cccccc;"/> </td> </tr> <tr><td>&nbsp;</td></tr> <tr> <td class="mid-head">Message : </td> <td> <textarea style=" width:200px; border: 1px solid #cccccc;"></textarea> </td></tr> </table> <p>&nbsp;</p> <center><button style="background:#449ed0;padding: 2px;color:white;">Send</button></center> <p>&nbsp;</p> </div> <p>&nbsp;</p> <p>&nbsp;</p> <p>&nbsp;</p>';
	else
		temp = '<div class="message-cont"> <div class="mess-sender"> Sent By : Sumeet Cheetri <b style="float:right;color:#cccccc;margin-right:3px;"> 19/9/2009 </b> </div> <p>&nbsp;</p> <div class="message"> ssssssssssssssssssssssssssssssssssssssssssss sssssssssssssssssssssssssssssssssssssssss ssssssssssssssssssssssssssssssssssssssssssss sssssssssssssssssssssss ssssssssssssssssssssssssssssssssssssssssssss sssssssssssssssssssssssssssssssssssssssss ssssssssssssssssssssssssssssssssssssssssssss sssssssssssssssssssssss ssssssssssssssssssssssssssssssssssssssssssss sssssssssssssssssssssssssssssssssssssssss ssssssssssssssssssssssssssssssssssssssssssss sssssssssssssssssssssss ssssssssssssssssssssssssssssssssssssssssssss sssssssssssssssssssssssssssssssssssssssss ssssssssssssssssssssssssssssssssssssssssssss sssssssssssssssssssssss sssssssssssssssssssssssssssssssssssssssss ssssssssssssssssssssssssssssssssssssssssssss sssssssssssssssssssssss </div> <p>&nbsp;</p> <center> <button style="background:#449ed0;padding:2px;color:white; margin:5px;">Forward</button> <button style="background:#449ed0;padding: 2px;color:white;margin:5px;">Reply</button> <button style="background:#449ed0;padding: 2px;color:white;margin:5px;">Delete</button> </center> <p>&nbsp;</p> </div>';
	$('rightContentHolder').innerHTML = temp;
}
function paintGallery(type)
{
	var temp = '<div class="invitemenu"> <ul> <li><a href="#" onclick="paintGallery(\'Create\')">Create</a></li> <li><a href="#" onclick="paintGallery(\'View\')">View</a></li></ul> </div>';
	if(type=='View')
		temp += '<div class="gallery-box"> <p>&nbsp;</p> <p class="gray-back"><a href="#"class="mid-head" onclick="paintGallery(\'\')">Home (41)</a><b style="float:right;">Description comes here first line only</b></p> <p class="gray-back"><a href="#"class="mid-head" onclick="paintGallery(\'\')">Friends (41)</a><b style="float:right;">Description comes here first line only</b></p>	<p class="gray-back"><a href="#"class="mid-head" onclick="paintGallery(\'\')">Picnic (41)</a><b style="float:right;">Description comes here first line only</b></p>	<p>&nbsp;</p> <p>&nbsp;</p> <p>&nbsp;</p> </div> <p>&nbsp;</p> <p>&nbsp;</p> <p>&nbsp;</p></div>';
	else if(type=='Create')
		temp += '<div class="gallery-box"> <p>&nbsp;</p> <table style="margin-left:30%;"> <tr align="left"> <td class="mid-head">Name : </td> <td><input type="text" style="width:200px; border: 1px solid #cccccc;"/></td> </tr> <tr><td>&nbsp;</td></tr> <tr> <td class="mid-head">Category : </td> <td> <SELECT style="border: 1px solid #cccccc;"> <OPTION VALUE=""/>Category <OPTION VALUE="1"/>Private <OPTION VALUE="2"/>Public </SELECT> </td> </tr> <tr><td>&nbsp;</td></tr> <tr> <td class="mid-head">Description : </td> <td> <textarea style=" width:200px; border: 1px solid #cccccc;"></textarea> </td></tr> </table> <p>&nbsp;</p> <center><button style="background:#449ed0;padding: 2px;color:white;">Create</button></center> <p>&nbsp;</p> </div> <p>&nbsp;</p> <p>&nbsp;</p> <p>&nbsp;</p></div>';
	else
		temp = '<div class="message-cont"> <div class="mess-sender"> Album Name : Home(41) <a href="#"><b style="float:right;color:#cccccc;margin-right:3px;"> Upload Images</b></a> </div> <p>&nbsp;</p> <center> <table width="94%" style="margin:3%;"> <tr> <td width="1%"></td> <td width="18%"> <img src="img/photo.gif"style="border:1px solid black"/></br> <span><center><a href="#">Veiw</a> | <a href="#">Delete</a> </center></span> </td> <td width="1%"></td> <td width="18%"> <img   src="img/photo.gif"style="border:1px solid black"/> </br> <span><center><a href="#">Veiw</a> | <a href="#">Delete</a></center></span> </td> <td width="1%"></td> <td width="18%"> <img  src="img/photo.gif"style="border:1px solid black"/> </br> <span><center><a href="#">Veiw</a> | <a href="#">Delete</a></center></span> </td> <td width="1%"></td> <td width="18%"> <img  src="img/photo.gif"style="border:1px solid black"/> </br> <span><center><a href="#">Veiw</a> | <a href="#">Delete</a></center></span> </td> <td width="1%"></td> <td width="18%"> <img  src="img/photo.gif"style="border:1px solid black"/> </br> <span><center><a href="#">Veiw</a> | <a href="#">Delete</a></center></span> </td> <td width="1%"></td> </tr> </table> </center> <p>&nbsp;</p> <center> <p>&nbsp;</p> </div>';
	$('rightContentHolder').innerHTML = temp;
}
function getDateSelect(frmYr)
{
	var temp = '<SELECT style="width:50px;margin-top:5px;border:1px solid #eaeaea;"><OPTION VALUE=""/>DD';
	for(var i=1;i<32;i++)
		temp += '<OPTION VALUE="'+i+'"/>'+i;
	temp += '</SELECT>&nbsp;&nbsp;<SELECT id="sec_qs" style="width:50px;margin-top:5px;border: 1px solid #eaeaea;"><OPTION VALUE=""/>MMM<OPTION VALUE="1"/>Jan <OPTION VALUE="2"/>Feb<OPTION VALUE="3"/>Mar<OPTION VALUE="4"/>Apr<OPTION VALUE="5"/>May<OPTION VALUE="6"/>Jun <OPTION VALUE="7"/>Jul <OPTION VALUE="8"/>  Aug <OPTION VALUE="9"/>Sep<OPTION VALUE="10"/>Oct<OPTION VALUE="11"/>Nov<OPTION VALUE="12"/>Dec</SELECT>&nbsp;&nbsp;<SELECT id="sec_qs" style="width:50px;margin-top:5px;border: 1px solid #eaeaea;"> <OPTION VALUE=""/>YYYY';
	for(var i=frmYr;i<2000;i++)
		temp += '<OPTION VALUE="'+i+'"/>'+i;
	temp += '</SELECT>';
	return temp;
}

/**
 *  The debugger tool developed by Nikhil Sidhaye for internal use.
 */
 
var _console = null;
var _debug = true;
 
 
function log(obj) {
	log(obj, false);
}
/*
 * For evaluating.
 */ 
function log(obj, isEval) {
	var color = "";
	var result = null;
 
	if(!_debug) {
		return;
	}
	
	getLogWindow();
 
	// evaluate.
 
	if(isEval) {
		if(obj == "undefined") {
			_console.document.writeln("<font color='red'><B>");
		}
 
		_console.document.writeln("<BR>Evaluating : " + obj);
		
		if(obj == "undefined") {
			_console.document.writeln("</B></font>");
		}
		_console.document.write("<BR>Ans : ");
		
		result = eval(obj);
		print(result);		
	}else {
		_console.document.writeln("<BR>Logging : " + obj);
	}
}
 
function print(obj) {
	
	if(obj == "undefined" || obj == null) {
		_console.document.writeln("<font color='red'><B>");
 
		_console.document.writeln(obj);
 
		_console.document.writeln("</B></font>");
	} else {
		_console.document.writeln(obj);
	}
	
	_console.document.writeln("<BR>Type: " + typeof(obj));
	_console.document.writeln("<HR size='1' color='blue'>")
}
 
 
function logFunction() {
	var obj = document.getElementById('debug01');
	log(obj.value, true);
}
 
function getLogWindow() {
	if(_console == null) {
			_console = window.open("",'Debug_Window', 'width=300,height=200,menubar=no,status=yes,resizable=yes,location=no,toolbar=no,scrollbars=yes');
		} else {
			if(_console.closed) {
				_console = window.open("",'Debug_Window', 'width=300,height=200,menubar=no,status=yes,resizable=yes,location=no,toolbar=no,scrollbars=yes');
			}else {
				_console.focus();
			}
	}
}