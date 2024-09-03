/* WebFormsJS 1.2 - Providing Infrastructure For Web Controls In CodeBehind Framework Owned By Elanat (elanat.net) */

/* Start Options */

var PostBackOptions = new Object();
PostBackOptions.UseProgressBar = true;
PostBackOptions.UseConnectionErrorMessage = true;
PostBackOptions.ConnectionErrorMessage = "Connection Error";
PostBackOptions.AutoSetSubmitOnClick = true;
PostBackOptions.SendDataOnlyByPostMethod = false;
PostBackOptions.ResponseLocation = null;
PostBackOptions.WebFormsTagsBackgroundColor = "#eee";
PostBackOptions.SetResponseInsideDivTag = true;

function cb_SetResponseLocation()
{
	PostBackOptions.ResponseLocation = document.body;
}

/* End Options */

/* Start Event */

function cb_SetPostBackFunctionToSubmit(obj)
{
    if (!PostBackOptions.AutoSetSubmitOnClick)
        return;

    const SubmitInputs = (obj) ? obj.querySelectorAll('input[type="submit"]') : document.querySelectorAll('input[type="submit"]');

    SubmitInputs.forEach(function (InputElement)
    {
        if (InputElement.hasAttribute("onclick"))
        {
            var OnClickAttr = InputElement.getAttribute("onclick");

            if (!OnClickAttr)
            {
                InputElement.setAttribute("onclick", "PostBack(this)");
                return;
            }

            if (!OnClickAttr.ContainsNameWithSpliter("PostBack", ';', '('))
                if (OnClickAttr.charAt(OnClickAttr.length - 1) == ';')
                    InputElement.setAttribute("onclick", OnClickAttr + "PostBack(this)");
                else
                    InputElement.setAttribute("onclick", OnClickAttr + ";PostBack(this)");
        }
        else
            InputElement.setAttribute("onclick", "PostBack(this)");
    });
}

window.onload = function ()
{
	cb_SetResponseLocation();
    cb_Initialization();
};

function cb_Initialization(obj)
{
    if (obj)
    {
        cb_SetPostBackFunctionToSubmit(obj);
        cb_SetWebFormsTagsValue(obj);
    }
    else
    {
        cb_SetPostBackFunctionToSubmit();
        cb_SetWebFormsTagsValue();
    }
}

/* End Event */

/* Start Post-Back */

function PostBack(obj, ViewState)
{
    // Set Form Value
    var Form = obj;
    do
    {
        if (!Form.parentNode)
            return;

        Form = Form.parentNode;
    }
    while (Form.nodeName.toLowerCase() != "form");

    if (Form.nodeName.toLowerCase() != "form")
        return;

    var FormMethod = (Form.hasAttribute("method") && !PostBackOptions.SendDataOnlyByPostMethod) ? Form.getAttribute("method") : "POST" ;
    var FormAction = Form.getAttribute("action");

    // Chek Form Multi Part
    var FormIsMultiPart = false;
    if (Form.hasAttribute("enctype") && FormMethod.toLowerCase() == "post")
        if (Form.getAttribute("enctype") == "multipart/form-data")
            FormIsMultiPart = true;


    // Set Progress Tag
    if (PostBackOptions.UseProgressBar)
        cb_SetProgressTag(obj, Form);


    // Set Input Value
    var TagSubmitValue = null;
    switch (obj.nodeName.toLowerCase())
    {
        case "input": TagSubmitValue = (obj.getAttribute("value")) ? obj.getAttribute("value") : ""; break;
        case "select": TagSubmitValue =  (obj.options[obj.selectedIndex].value) ? obj.options[obj.selectedIndex].value : "";
    }

    var OldObjectType;
    if (obj.getAttribute("type"))
        if (obj.getAttribute("type") != "button")
        {
            OldObjectType = obj.type;
            obj.setAttribute("type", "button");
        }


    var XMLHttp = new XMLHttpRequest();
    XMLHttp.onreadystatechange = function ()
    {
        if (XMLHttp.readyState == 4 && XMLHttp.status == 200)
        {
            var HttpResult = XMLHttp.responseText;
            var IsWebForms = false;

            // Check Exist WebForms Values
            if (HttpResult.length >= 11)
                if (HttpResult.substring(0, 11) == "[web-forms]")
                    IsWebForms = true;

            if (IsWebForms)
                cb_SetWebFormsValues(HttpResult, true);
            else
            {
                var TmpDiv = document.createElement("div");
                TmpDiv.innerHTML = HttpResult.toDOM();
                cb_AppendJavaScriptTag(HttpResult);

                if (ViewState)
                {
                    if (typeof ViewState === "string")
                    {
                        var ViewStateObject = cb_GetElementByElementPlace(ViewState);
                        ViewStateObject.innerHTML = TmpDiv.outerHTML;
                        cb_Initialization(ViewStateObject.getElementsByTagName("div")[0]);
                        if (!PostBackOptions.SetResponseInsideDivTag)
                            ViewStateObject.getElementsByTagName("div")[0].outerHTML = ViewStateObject.getElementsByTagName("div")[0].innerHTML;
                    }
                    else if (typeof ViewState === "object")
                    {
                        ViewState.innerHTML = TmpDiv.outerHTML;
                        cb_Initialization(ViewState.getElementsByTagName("div")[0]);
                        if (!PostBackOptions.SetResponseInsideDivTag)
                            ViewState.getElementsByTagName("div")[0].outerHTML = ViewState.getElementsByTagName("div")[0].innerHTML;
                    }
                    else
                    {
                        PostBackOptions.ResponseLocation.prepend(TmpDiv);
                        cb_Initialization(PostBackOptions.ResponseLocation.getElementsByTagName("div")[0]);
                        if (!PostBackOptions.SetResponseInsideDivTag)
                            PostBackOptions.ResponseLocation.getElementsByTagName("div")[0].outerHTML = PostBackOptions.ResponseLocation.getElementsByTagName("div")[0].innerHTML;
                    }
                }
                else
                {
                    PostBackOptions.ResponseLocation.innerHTML = (PostBackOptions.SetResponseInsideDivTag) ? TmpDiv.outerHTML : TmpDiv.innerHTML;
                    cb_Initialization(PostBackOptions.ResponseLocation);
                }

                Form.focus();
            }

            // Reset Input Type
            setTimeout(function () { (OldObjectType == "submit") ? obj.type = "submit" : obj.type; }, 1);
        }
    }

    XMLHttp.onerror = function ()
    {
        if (XMLHttp.status != 0 && (XMLHttp.readyState == 0 || XMLHttp.status > 200))
        {
            if (PostBackOptions.UseConnectionErrorMessage)
            {
                var BErrorTag = document.createElement("b");
                BErrorTag.innerText = "Connection Error";
                document.body.prepend(BErrorTag);
            }

            // Clean Progress Value
            if (PostBackOptions.UseProgressBar)
                cb_CleanProgressValue();
        }

        // Reset Input Type
        setTimeout(function () { (OldObjectType == "submit") ? obj.type = "submit" : obj.type; }, 1);
    }

    XMLHttp.open(FormMethod, FormAction, true);

    if (PostBackOptions.UseProgressBar && cb_HasFileInput(Form))
        XMLHttp.upload.addEventListener("progress", cb_ProgressHandler, false);

    if (!FormIsMultiPart)
        XMLHttp.setRequestHeader("Content-Type", "application/x-www-form-urlencoded");

    XMLHttp.setRequestHeader("Post-Back", "true");

    XMLHttp.send(cb_FormDataSerialize(Form, obj.getAttribute("name"), TagSubmitValue, FormIsMultiPart));
}

/* End Post-Back */

/* Start Get-Back */

function GetBack(FormAction, ViewState)
{
    var FormMethod = (PostBackOptions.SendDataOnlyByPostMethod) ? "POST" : "GET";

    if (FormAction)
    {
        if (typeof FormAction === "object")
        {
            // Set Form Value
            var Form = FormAction;
            do
            {
                if (!Form.parentNode)
                    return;

                Form = Form.parentNode;
            }
            while (Form.nodeName.toLowerCase() != "form");

            if (Form.nodeName.toLowerCase() != "form")
                if (body.getElementsByTagName("form").length > 0)
                    Form = body.getElementsByTagName("form")[0];

            FormMethod = (Form.hasAttribute("method") && !PostBackOptions.SendDataOnlyByPostMethod) ? Form.getAttribute("method") : "POST" ;
            FormAction = Form.getAttribute("action");
        }
    }
    else
        FormAction = "";

    var XMLHttp = new XMLHttpRequest();
    XMLHttp.onreadystatechange = function ()
    {
        if (XMLHttp.readyState == 4 && XMLHttp.status == 200)
        {
            var HttpResult = XMLHttp.responseText;
            var IsWebForms = false;

            // Check Exist WebForms Values
            if (HttpResult.length >= 11)
                if (HttpResult.substring(0, 11) == "[web-forms]")
                    IsWebForms = true;

            if (IsWebForms)
                cb_SetWebFormsValues(HttpResult, true);
            else
            {
                var TmpDiv = document.createElement("div");
                TmpDiv.innerHTML = HttpResult.toDOM();
                cb_AppendJavaScriptTag(HttpResult);

                if (ViewState)
                {
                    if (typeof ViewState === "string")
                    {
                        var ViewStateObject = cb_GetElementByElementPlace(ViewState);
                        ViewStateObject.innerHTML = TmpDiv.outerHTML;
                        cb_Initialization(ViewStateObject.getElementsByTagName("div")[0]);
                        if (!PostBackOptions.SetResponseInsideDivTag)
                            ViewStateObject.getElementsByTagName("div")[0].outerHTML = ViewStateObject.getElementsByTagName("div")[0].innerHTML;
                    }
                    else if (typeof ViewState === "object")
                    {
                        ViewState.innerHTML = TmpDiv.outerHTML;
                        cb_Initialization(ViewState.getElementsByTagName("div")[0]);
                        if (!PostBackOptions.SetResponseInsideDivTag)
                            ViewState.getElementsByTagName("div")[0].outerHTML = ViewState.getElementsByTagName("div")[0].innerHTML;
                    }
                    else
                    {
                        PostBackOptions.ResponseLocation.prepend(TmpDiv);
                        cb_Initialization(PostBackOptions.ResponseLocation.getElementsByTagName("div")[0]);
                        if (!PostBackOptions.SetResponseInsideDivTag)
                            PostBackOptions.ResponseLocation.getElementsByTagName("div")[0].outerHTML = PostBackOptions.ResponseLocation.getElementsByTagName("div")[0].innerHTML;
                    }
                }
                else
                {
                    PostBackOptions.ResponseLocation.innerHTML = (PostBackOptions.SetResponseInsideDivTag) ? TmpDiv.outerHTML : TmpDiv.innerHTML;
                    cb_Initialization(PostBackOptions.ResponseLocation);
                }
            }
        }
    }

    XMLHttp.onerror = function ()
    {
        if (XMLHttp.status != 0 && (XMLHttp.readyState == 0 || XMLHttp.status > 200))
        {
            if (PostBackOptions.UseConnectionErrorMessage)
            {
                var BErrorTag = document.createElement("b");
                BErrorTag.innerText = "Connection Error";
                document.body.prepend(BErrorTag);
            }
        }
    }

    XMLHttp.open(FormMethod, FormAction, true);

    XMLHttp.setRequestHeader("Content-Type", "application/x-www-form-urlencoded");

    XMLHttp.setRequestHeader("Post-Back", "true");

    XMLHttp.send();
}

/* End Get-Back */

function cb_FormDataSerialize(form, TagSubmitName, TagSubmitValue, FormIsMultiPart)
{       
    var FormString = "";
    var TmpFormData = new FormData();

    if (!form || form.nodeName.toLowerCase() != "form")
        return;

    var i, j;
    for (i = form.elements.length - 1; i >= 0; i = i - 1)
    {
        if (form.elements[i].name === "")
            continue;

        switch (form.elements[i].nodeName.toLowerCase())
        {
            case 'input':
                switch (form.elements[i].type.toLowerCase())
                {
                    case 'text':
                    case 'number':
                    case 'hidden':
                    case 'password':
                    case 'reset':
                    case 'color':
                    case 'date':
                    case 'range':
                    case 'search':
                    case 'time':
                    case 'datetime-local':
                    case 'email':
                    case 'month':
                    case 'tel':
                    case 'url':
                    case 'week':
                        {
                            if (FormIsMultiPart)
                                TmpFormData.append(form.elements[i].name, form.elements[i].value);
                            else
                                FormString += form.elements[i].name + "=" + form.elements[i].value + "&";
                        }
                        break;
                    case 'checkbox':
                    case 'radio':
                        if (form.elements[i].checked)
                        {
                            if (FormIsMultiPart)
                                TmpFormData.append(form.elements[i].name, form.elements[i].value);
                            else
                                FormString += form.elements[i].name + "=" + form.elements[i].value + "&";
                        }
                        break;
                    case 'file':
                        {
                            var files = form.elements[i].files;

                            if (files.length == 0)
                                break;

                            var file = files[0];

                            if (FormIsMultiPart)
                                TmpFormData.append(form.elements[i].name, file, file.name);
                            else
                                FormString += form.elements[i].name + "=" + file, file.name + "&";
                        }
                        break;
                }
                break;
            case 'file':
                break;
            case 'textarea':
                {
                    if (FormIsMultiPart)
                        TmpFormData.append(form.elements[i].name, form.elements[i].value);
                    else
                        FormString += form.elements[i].name + "=" + form.elements[i].value + "&";
                }
                break;
            case 'select':
                switch (form.elements[i].type.toLowerCase())
                {
                    case 'select-one':
                        {
                            if (FormIsMultiPart)
                                TmpFormData.append(form.elements[i].name, form.elements[i].value);
                            else
                                FormString += form.elements[i].name + "=" + form.elements[i].value + "&";
                        }
                        break;
                    case 'select-multiple':
                        for (j = form.elements[i].options.length - 1; j >= 0; j = j - 1)
                        {
                            if (form.elements[i].options[j].selected)
                            {
                                if (FormIsMultiPart)
                                    TmpFormData.append(form.elements[i].name, form.elements[i].options[j].value);
                                else
                                    FormString += form.elements[i].name + "=" + form.elements[i].options[j].value + "&";
                            }
                        }
                        break;
                }
                break;
            case 'button':
                switch (form.elements[i].type.toLowerCase())
                {
                    case 'reset':
                    case 'submit':
                    case 'button':
                        {
                            if (FormIsMultiPart)
                                TmpFormData.append(form.elements[i].name, form.elements[i].value);
                            else
                                FormString += form.elements[i].name + "=" + form.elements[i].value + "&";
                        }
                        break;
                }
                break;
        }
    }

    if (FormIsMultiPart)
        TmpFormData.append(TagSubmitName, TagSubmitValue);
    else
        FormString += TagSubmitName + "=" + TagSubmitValue;

    return (FormIsMultiPart) ? TmpFormData : FormString;
}

/* Start Append Java Script */

function cb_ExtractScriptTags(Html)
{
    var ScriptList = new Array();
    const regex = /<script[^>]*>([\s\S]*?)<\/script>/g;
    let match;

    while ((match = regex.exec(Html)) !== null)
    {
        const ScriptTag = document.createElement("script");
        const ScriptContent = match[1];

        // Extract Attributes
        const AttrRegex = /([a-zA-Z0-9_]+)="([^"]*)"/g;
        let AttrMatch;

        while ((AttrMatch = AttrRegex.exec(match[0])) !== null)
        {
            const Name = AttrMatch[1];
            const Value = AttrMatch[2];
            ScriptTag.setAttribute(Name, Value);
        }

        const TextNode = document.createTextNode(ScriptContent);

        ScriptTag.appendChild(TextNode);
        ScriptList.push(ScriptTag);
    }

    return ScriptList;
}

function cb_AppendJavaScriptTag(HtmlSource)
{
    var ScriptList = cb_ExtractScriptTags(HtmlSource);

    for (var i = 0; i < ScriptList.length; i++)
        document.body.appendChild(ScriptList[i]);
}

/* End Append Java Script */

/* Start Progress Bar */
function cb_ProgressHandler(event)
{
    var Percent = (event.loaded / event.total) * 100;

    if (event.total >= 1048576)
        document.getElementById("div_ProgressPercentLoaded").innerHTML = (event.loaded / 1048576).toFixed(1) + "(" + Math.round(Percent) + "%)" + " / " + (event.total / 1048576).toFixed(1) + " MB";
    else
        document.getElementById("div_ProgressPercentLoaded").innerHTML = (event.loaded / 1024).toFixed(1) + "(" + Math.round(Percent) + "%)" + " / " + (event.total / 1024).toFixed(1) + " KB";

    document.getElementById("div_ProgressUploadValue").style.width = Math.round(Percent) + "%";
}

function cb_SetProgressTag(obj, form)
{
    if (!cb_HasFileInput(form))
        return;

    if (!document.getElementById("div_ProgressUpload"))
    {
        var DivProgressUpload = document.createElement("div");
        DivProgressUpload.id = "div_ProgressUpload";
        DivProgressUpload.setAttribute("style", "width:100%;min-width:300px;max-width:600px;background-color:#eee;margin:2px 0px");

        var DivProgressPercentLoaded = document.createElement("div");
        DivProgressPercentLoaded.id = "div_ProgressPercentLoaded";
        DivProgressPercentLoaded.setAttribute("style", "position:absolute;padding:0px 4px;line-height:22px");

        var DivProgressUploadValue = document.createElement("div");
        DivProgressUploadValue.id = "div_ProgressUploadValue";
        DivProgressUploadValue.setAttribute("style", "height:20px;background-color:#4D93DD;width:0%");

        DivProgressUpload.appendChild(DivProgressPercentLoaded);
        DivProgressUpload.appendChild(DivProgressUploadValue);

        if (obj.parentElement)
            obj.parentElement.appendChild(DivProgressUpload);
        else
            document.body.appendChild(DivProgressUpload);
    }
}

function cb_CleanProgressValue()
{
    if (document.getElementById("div_ProgressUploadValue"))
        document.getElementById("div_ProgressUpload").outerHTML = "";
}

function cb_HasFileInput(Form)
{
    if (Form.getElementsByTagName("file").length > 0)
        return true;

    var InputCount = Form.getElementsByTagName("input").length;

    for (var i = 0; i < InputCount; i++)
        if (Form.getElementsByTagName("input").item(i).hasAttribute("type"))
            if (Form.getElementsByTagName("input").item(i).getAttribute("type") == "file")
                return true;

    return false;
}

/* End Progress Bar */

/* Start Web-Forms Tags */

function cb_SetWebFormsTagsValue(obj)
{
    const WebFormsTags = (obj) ? obj.querySelectorAll('web-forms') : document.querySelectorAll('web-forms');

    WebFormsTags.forEach(function (WebForms)
    {
        if (WebForms.hasAttribute("src"))
        {
            WebForms.style.backgroundColor = PostBackOptions.WebFormsTagsBackgroundColor;
            if (WebForms.hasAttribute("width"))
                WebForms.style.width = WebForms.getAttribute("width");
            if (WebForms.hasAttribute("height"))
                WebForms.style.height = WebForms.getAttribute("height");

            var Src = WebForms.getAttribute("src");
            if (Src)
                GetBack(Src, WebForms);

            WebForms.style.backgroundColor = "unset";
        }

        if (WebForms.hasAttribute("ac"))
        {
            var ActionControl = WebForms.getAttribute("ac");
            if (ActionControl)
                cb_SetWebFormsValues(ActionControl.Replace("$[dq];","\""), false, true);
        }
    });
}

/* End Web-Forms Tags */

/* Start Fetch Web-Forms */

function cb_SetWebFormsValues(WebFormsValues, UsePostBack, WithoutWebFormsSection)
{
    if (!WithoutWebFormsSection)
        WebFormsValues = WebFormsValues.substring(11);

    var WebFormsList = (UsePostBack) ? WebFormsValues.split('\n') : WebFormsValues.split("$[sln];");

    for (var i = 0; i < WebFormsList.length; i++)
    {
        if (!WebFormsList[i].FullTrim())
            continue;

        var PreRunner = new Array();
        var FirstChar = WebFormsList[i].substring(0, 1);
        var PreRunnerIndexer = 0;
        while ((FirstChar == ':') || (FirstChar == '('))
        {
            PreRunner[PreRunnerIndexer++] = WebFormsList[i].GetTextBefore(")");
            WebFormsList[i] = WebFormsList[i].GetTextAfter(")");
            FirstChar = WebFormsList[i].substring(0, 1);
        }

        switch (FirstChar)
        {
            case '_':
                var ScriptValue = WebFormsList[i].GetTextAfter("=").Replace("$[ln];", "\n").FullTrim();
                cb_SetPreRunnerQueueForEval(PreRunner, ScriptValue);
                continue;
        }


        var ActionName = WebFormsList[i].substring(0, 2);
        var ActionValue = WebFormsList[i].substring(2);

        var ActionOperation = ActionName.substring(0, 1);
        var ActionFeature = ActionName.substring(1, 2);

        cb_SetPreRunnerQueueForSetValueToInput(PreRunner, ActionOperation, ActionFeature, ActionValue);
    }
}

function cb_SetValueToInput(ActionOperation, ActionFeature, ActionValue)
{
    var ElementPlace = ActionValue.GetTextBefore("=");
    var Value = ActionValue.GetTextAfter("=").FullTrim();
    var LabelForIndexer = 0;

    var ElementPlaceList;

    if (ElementPlace.substring(0, 1) == '[')
    {
        var QueryAll = ElementPlace.substring(1);
        ElementPlaceList = document.querySelectorAll(QueryAll.Replace("$[eq];", "="));
    }
    else
    {
        ElementPlaceList = new Array();
        ElementPlaceList[0] = cb_GetElementByElementPlace(ElementPlace);
    }

    for (var i = 0; i < ElementPlaceList.length; i++)
    {
        CurrentElement = ElementPlaceList[i];

        if (!CurrentElement)
            continue;

        // Without Server Attribute
        switch (ActionOperation)
        {
            case 'a':
                switch (ActionFeature)
                {
                    case 'i': CurrentElement.id = (CurrentElement.id) ? CurrentElement.id + Value : Value; break;
                    case 'n':
                        if (CurrentElement.tagName.IsInput())
                            CurrentElement.name = (CurrentElement.name) ? CurrentElement.name + Value : Value;
                        else
                            if (CurrentElement.hasAttribute("name"))
                            {
                                var NameAttr = CurrentElement.getAttribute("name");
                                CurrentElement.setAttribute("name", NameAttr + Value);
                            }
                            else
                                CurrentElement.setAttribute("name", Value);
                        break;
                    case 'v':
                        if (CurrentElement.tagName.IsInput())
                            CurrentElement.value = (CurrentElement.value) ? CurrentElement.value + Value : Value;
                        else
                            if (CurrentElement.hasAttribute("value"))
                            {
                                var ValueAttr = CurrentElement.getAttribute("value");
                                CurrentElement.setAttribute("value", ValueAttr + Value);
                            }
                            else
                                CurrentElement.setAttribute("value", Value);
                        break;
                    case 'c':
                        if (CurrentElement.hasAttribute("class"))
                        {
                            var ClassAttr = CurrentElement.getAttribute("class");
                            CurrentElement.setAttribute("class", ClassAttr + ' ' + Value);
                        }
                        else
                            CurrentElement.setAttribute("class", Value);
                        break;
                    case 's':
                        if (CurrentElement.hasAttribute("style"))
                        {
                            var StyleAttr = CurrentElement.getAttribute("style");
                            if (StyleAttr.charAt(StyleAttr.length - 1) == ';')
                                CurrentElement.setAttribute("style", StyleAttr + Value);
                            else
                                CurrentElement.setAttribute("style", StyleAttr + ';' + Value);
                        }
                        else
                            CurrentElement.setAttribute("style", Value);
                        break;
                    case 'o':
                        var OptionTag = document.createElement("option");
                        var OptionValue = Value.GetTextBefore("|");
                        var OptionText = Value.GetTextAfter("|");
                        if (OptionText.Contains("|"))
                        {
                            OptionTag.selected = (OptionText.GetTextAfter("|") == "1");
                            OptionText = OptionText.GetTextBefore("|");
                        }

                        OptionTag.value = OptionValue;
                        OptionTag.text = OptionText;

                        CurrentElement.appendChild(OptionTag);
                        break;
                    case 'k':
                        var CheckBoxTag = document.createElement("input");
                        CheckBoxTag.setAttribute("type", "checkbox");

                        var CheckBoxValue = Value.GetTextBefore("|");
                        var CheckBoxText = Value.GetTextAfter("|");
                        if (CheckBoxText.Contains("|"))
                        {
                            CheckBoxTag.checked = (CheckBoxText.GetTextAfter("|") == "1");
                            CheckBoxText = CheckBoxText.GetTextBefore("|");
                        }

                        CheckBoxTag.setAttribute("value", CheckBoxValue);
                        var CeckBoxIndex = CurrentElement.querySelectorAll('input[type="checkbox"]').length;

                        var CheckBoxNameAndText = "cblst_NoneSet";
                        if (CurrentElement.id)
                            CheckBoxNameAndText = CurrentElement.id;
                        else
                            if (CeckBoxIndex > 0)
                                CheckBoxNameAndText = CurrentElement.querySelectorAll('input[type="checkbox"]')[0].name.GetTextBefore("$");

                        CheckBoxTag.id = CheckBoxNameAndText + "_" + CeckBoxIndex;
                        CheckBoxTag.name = CheckBoxNameAndText + "$" + CeckBoxIndex;

                        CurrentElement.appendChild(document.createElement("br"));

                        CurrentElement.appendChild(CheckBoxTag);

                        var LabelTag = document.createElement("label");
                        LabelTag.setAttribute("for", CheckBoxTag.id);
                        LabelTag.innerText = CheckBoxText;
                        CurrentElement.appendChild(LabelTag);

                        break;
                    case 'l':
                        if (!CurrentElement.tagName.IsInput())
                        {
                            if (CurrentElement.hasAttribute("title"))
                            {
                                var TitleAttr = CurrentElement.getAttribute("title");
                                CurrentElement.setAttribute("title", TitleAttr + Value);
                            }
                            else
                                CurrentElement.setAttribute("title", Value);
                            break;
                        }

                        if (!CurrentElement.id)
                            CurrentElement.id = "tmp_Element" + LabelForIndexer++;

                        var LabelTag = document.querySelector('label[for="' + CurrentElement.id + '"]');

                        if (LabelTag)
                            LabelTag.innerText = LabelTag.innerText + Value;
                        else
                        {
                            LabelTag = document.createElement("label");
                            LabelTag.setAttribute("for", CurrentElement.id);
                            LabelTag.innerText = Value;
                            CurrentElement.outerHTML = CurrentElement.outerHTML + LabelTag.outerHTML;
                        }
                        break;
                    case 't':
                        CurrentElement.innerHTML = CurrentElement.innerHTML + Value.Replace("$[ln];", "\n").toDOM();
                        cb_Initialization(CurrentElement);
                        break;
                    case 'a':
                        var AttrName = Value;
                        var AttrValue = "";
                        if (Value.Contains("|"))
                        {
                            AttrName = Value.GetTextBefore("|");
                            AttrValue = Value.GetTextAfter("|");
                        }
                        if (CurrentElement.hasAttribute(AttrName))
                        {
                            var CurrentAttr = CurrentElement.getAttribute(AttrName);
                            if (CurrentAttr.charAt(CurrentAttr.length - 1) == ';')
                                CurrentElement.setAttribute(AttrName, CurrentAttr + AttrValue);
                            else
                                CurrentElement.setAttribute(AttrName, CurrentAttr + ';' + AttrValue);
                        }
                        else
                            CurrentElement.setAttribute(AttrName, AttrValue);
                }
                break;

            case 's':
            case 'i':
                switch (ActionFeature)
                {
                    case 'i':
                        if ((ActionOperation == 'i') && (CurrentElement.id))
                            break;

                        CurrentElement.id = Value;
                        break;
                    case 'n':
                        if (CurrentElement.tagName.IsInput())
                        {
                            if ((ActionOperation == 'i') && CurrentElement.name)
                                break;

                            CurrentElement.name = Value;
                        }
                        else
                        {
                            if (ActionOperation == 'i' && CurrentElement.hasAttribute("name"))
                                break;

                            CurrentElement.setAttribute("name", Value);
                        }
                        break;
                    case 'v':
                        if (CurrentElement.tagName.IsInput())
                        {
                            if ((ActionOperation == 'i') && CurrentElement.value)
                                break;

                            CurrentElement.value = Value;
                        }
                        else
                        {
                            if (ActionOperation == 'i' && CurrentElement.hasAttribute("value"))
                                break;

                            CurrentElement.setAttribute("value", Value);
                        }
                        break;
                    case 'c':
                        if (CurrentElement.hasAttribute("class"))
                        {
                            var ClassAttr = CurrentElement.getAttribute("class");

                            if ((ActionOperation == 'i') && (ClassAttr.ContainsWithSpliter(Value, " ")))
                                break;

                            CurrentElement.setAttribute("class", ClassAttr + ' ' + Value);
                        }
                        else
                            CurrentElement.setAttribute("class", Value);
                        break;
                    case 's':
                        if (CurrentElement.hasAttribute("style"))
                        {
                            var StyleAttr = CurrentElement.getAttribute("style");

                            if ((ActionOperation == 'i') && (StyleAttr.ContainsWithSpliter(Value, ";")))
                                break;

                            if (StyleAttr.charAt(StyleAttr.length - 1) == ';')
                                CurrentElement.setAttribute("style", StyleAttr + Value);
                            else
                                CurrentElement.setAttribute("style", StyleAttr + ';' + Value);
                        }
                        else
                            CurrentElement.setAttribute("style", Value);
                        break;
                    case 'o':
                        if ((ActionOperation == 'i') && (CurrentElement.querySelectorAll('option[value="' + Value.GetTextBefore("|") + ' "]').length > 0))
                            break;

                        var OptionTag = document.createElement("option");
                        var OptionValue = Value.GetTextBefore("|");
                        var OptionText = Value.GetTextAfter("|");
                        if (OptionText.Contains("|"))
                        {
                            OptionTag.selected = (OptionText.GetTextAfter("|") == "1");
                            OptionText = OptionText.GetTextBefore("|");
                        }

                        OptionTag.value = OptionValue;
                        OptionTag.text = OptionText;

                        CurrentElement.appendChild(OptionTag);
                        break;
                    case 'k':
                        if ((CurrentElement.tagName.toLowerCase() == "input") && ((CurrentElement.type.toLowerCase() == "checkbox") || (CurrentElement.type.toLowerCase() == "radio")))
                        {
                            CurrentElement.checked = (Value == "1");
                            break;
                        }

                        if ((ActionOperation == 'i') && (CurrentElement.querySelectorAll('input[type="checkbox"][value="' + Value.GetTextBefore("|") + '"]').length > 0))
                            break;

                        var CheckBoxTag = document.createElement("input");
                        CheckBoxTag.setAttribute("type", "checkbox");

                        var CheckBoxValue = Value.GetTextBefore("|");
                        var CheckBoxText = Value.GetTextAfter("|");
                        if (CheckBoxText.Contains("|"))
                        {
                            CheckBoxTag.checked = (CheckBoxText.GetTextAfter("|") == "1");
                            CheckBoxText = CheckBoxText.GetTextBefore("|");
                        }

                        CheckBoxTag.setAttribute("value", CheckBoxValue);
                        var CeckBoxIndex = CurrentElement.querySelectorAll('input[type="checkbox"]').length;

                        var CheckBoxNameAndText = "cblst_NoneSet";
                        if (CurrentElement.id)
                            CheckBoxNameAndText = CurrentElement.id;
                        else
                            if (CeckBoxIndex > 0)
                                CheckBoxNameAndText = CurrentElement.querySelectorAll('input[type="checkbox"]')[0].name.GetTextBefore("$");

                        CheckBoxTag.id = CheckBoxNameAndText + "_" + CeckBoxIndex;
                        CheckBoxTag.name = CheckBoxNameAndText + "$" + CeckBoxIndex;

                        CurrentElement.appendChild(document.createElement("br"));

                        CurrentElement.appendChild(CheckBoxTag);

                        var LabelTag = document.createElement("label");
                        LabelTag.setAttribute("for", CheckBoxTag.id);
                        LabelTag.innerText = CheckBoxText;
                        CurrentElement.appendChild(LabelTag);

                        break;
                    case 'l':
                        if (!CurrentElement.tagName.IsInput())
                        {
                            if (CurrentElement.hasAttribute("title"))
                            {
                                if ((ActionOperation == 'i') && CurrentElement.getAttribute("title"))
                                    break;

                                var TitleAttr = CurrentElement.getAttribute("title");
                                CurrentElement.setAttribute("title", TitleAttr + Value);
                            }
                            else
                                CurrentElement.setAttribute("title", Value);
                            break;
                        }

                        if (!CurrentElement.id)
                            CurrentElement.id = "tmp_Element" + LabelForIndexer++;

                        var LabelTag = document.querySelector('label[for="' + CurrentElement.id + '"]');

                        if (LabelTag)
                        {
                            if ((ActionOperation == 'i') && CurrentElement.innerText)
                                break;

                            LabelTag.innerText = Value;
                        }
                        else
                        {
                            LabelTag = document.createElement("label");
                            LabelTag.setAttribute("for", CurrentElement.id);
                            LabelTag.innerText = Value;
                            CurrentElement.outerHTML = CurrentElement.outerHTML + LabelTag.outerHTML;
                        }
                        break;
                    case 't':
                        if ((ActionOperation == 'i') && (CurrentElement.innerHTML || CurrentElement.innerText))
                            break;

                        CurrentElement.innerHTML = Value.Replace("$[ln];", "\n").toDOM();
                        cb_Initialization(CurrentElement);
                        break;
                    case 'a':
                        var AttrName = Value;
                        var AttrValue = "";
                        if (Value.Contains("|"))
                        {
                            AttrName = Value.GetTextBefore("|");
                            AttrValue = Value.GetTextAfter("|");
                        }
                        if (CurrentElement.hasAttribute(AttrName))
                        {
                            var CurrentAttr = CurrentElement.getAttribute(AttrName);

                            if ((ActionOperation == 'i') && (CurrentAttr.ContainsWithSpliter(AttrValue, ";")))
                                break;

                            if (CurrentAttr.charAt(CurrentAttr.length - 1) == ';')
                                CurrentElement.setAttribute(AttrName, CurrentAttr + AttrValue);
                            else
                                CurrentElement.setAttribute(AttrName, CurrentAttr + ';' + AttrValue);
                        }
                        else
                            CurrentElement.setAttribute(AttrName, AttrValue);
                }
                break;

            case 'd':
                switch (ActionFeature)
                {
                    case 'i':
                        if (CurrentElement.id && Value == "1")
                            CurrentElement.removeAttribute("id");
                        break;
                    case 'n':
                        if (CurrentElement.name && Value == "1")
                            CurrentElement.removeAttribute("name");
                        break;
                    case 'v':
                        if (CurrentElement.value && Value == "1")
                            CurrentElement.value = "";
                        break;
                    case 'c':
                        if (CurrentElement.className)
                            CurrentElement.className = CurrentElement.className.DeleteHtmlClass(Value);
                        break;
                    case 's':
                        if (CurrentElement.hasAttribute("style"))
                        {
                            var StyleAttr = CurrentElement.getAttribute("style").DeleteHtmlStyle(Value);
                            CurrentElement.setAttribute("style", StyleAttr);
                        }
                        break;
                    case 'o':
                        if (CurrentElement.querySelectorAll('option[value="' + Value + '"]').length > 0)
                            CurrentElement.querySelectorAll('option[value="' + Value + '"]')[0].outerHTML = "";
                        break;
                    case 'k':
                        var CheckBoxTagLength = CurrentElement.querySelectorAll('input[type="checkbox"][value="' + Value + '"]').length;
                        if (CheckBoxTagLength > 0)
                        {
                            var CheckBoxTag = CurrentElement.querySelectorAll('input[type="checkbox"][value="' + Value + '"]')[0];
                            if (CheckBoxTag.id)
                                if (CurrentElement.querySelectorAll('label[for="' + CheckBoxTag.id + '"]').length > 0)
                                    CurrentElement.querySelectorAll('label[for="' + CheckBoxTag.id + '"]')[0].outerHTML = "";

                            CheckBoxTag.outerHTML = "";
                        }
                        break;
                    case 'l':
                        if (!CurrentElement.tagName.IsInput())
                        {
                            if (CurrentElement.hasAttribute("title") && Value == "1")
                                CurrentElement.removeAttribute("title");

                            break;
                        }

                        if (CurrentElement.id)
                        {
                            var LabelTag = document.querySelector('label[for="' + CurrentElement.id + '"]');
                            if (LabelTag)
                                LabelTag.outerHTML = "";
                        }

                        break;
                    case 't':
                        if (Value == "1")
                            CurrentElement.innerHTML = "";
                        break;
                    case 'a':
                        if (CurrentElement.hasAttribute(Value))
                            CurrentElement.removeAttribute(Value);

                        break;
                    case 'e':
                        if (Value == "1")
                            CurrentElement.outerHTML = "";
                }
                break;

            case '+':
            case '-':
                switch (ActionFeature)
                {
                    case "n":
                        if (CurrentElement.hasAttribute("minlength"))
                        {
                            var ElementMinLength = (ActionOperation == '+') ? parseInt(CurrentElement.getAttribute("minlength")) + parseInt(Value) : parseInt(CurrentElement.getAttribute("minlength")) - parseInt(Value);
                            CurrentElement.setAttribute("minlength", ElementMinLength);
                        }
                        else
                            if ((ActionOperation == '+'))
                                CurrentElement.setAttribute("minlength", Value);
                        break;
                    case "x":
                        if (CurrentElement.hasAttribute("maxlength"))
                        {
                            var ElementMaxLength = (ActionOperation == '+') ? parseInt(CurrentElement.getAttribute("maxlength")) + parseInt(Value) : parseInt(CurrentElement.getAttribute("maxlength")) - parseInt(Value);
                            CurrentElement.setAttribute("maxlength", ElementMaxLength);
                        }
                        else
                            if ((ActionOperation == '+'))
                                CurrentElement.setAttribute("maxlength", Value);
                        break;
                    case "f":
                        if (CurrentElement.style.fontSize)
                        {
                            var Unit = CurrentElement.style.fontSize.GetUnit();
                            var ElementFontSize = (ActionOperation == '+') ? parseInt(CurrentElement.style.fontSize) + parseInt(Value) : parseInt(CurrentElement.style.fontSize) - parseInt(Value);
                            CurrentElement.style.fontSize = ElementFontSize.toString() + Unit;
                        }
                        else
                            if ((ActionOperation == '+'))
                                CurrentElement.style.fontSize = Value + "px";
                        break;
                    case "w":
                        if (CurrentElement.style.width)
                        {
                            var Unit = CurrentElement.style.width.GetUnit();
                            var ElementWidth = (ActionOperation == '+') ? parseInt(CurrentElement.style.width) + parseInt(Value) : parseInt(CurrentElement.style.width) - parseInt(Value);
                            CurrentElement.style.width = ElementWidth.toString() + Unit;
                        }
                        else
                            if ((ActionOperation == '+'))
                                CurrentElement.style.width = Value + "px";
                        break;
                    case "h":
                        if (CurrentElement.style.height)
                        {
                            var Unit = CurrentElement.style.height.GetUnit();
                            var ElementHeight = (ActionOperation == '+') ? parseInt(CurrentElement.style.height) + parseInt(Value) : parseInt(CurrentElement.style.height) - parseInt(Value);
                            CurrentElement.style.height = ElementHeight.toString() + Unit;
                        }
                        else
                            if ((ActionOperation == '+'))
                                CurrentElement.style.height = Value + "px";
                        break;
                    case "v":
                        if (CurrentElement.value)
                        {
                            var ElementValue = (ActionOperation == '+') ? parseInt(CurrentElement.value) + parseInt(Value) : parseInt(CurrentElement.value) - parseInt(Value);
                            CurrentElement.value = ElementValue.toString();
                        }
                        else
                            if ((ActionOperation == '+'))
                                CurrentElement.value = Value;
                }
                break;
        }

        switch (ActionOperation + ActionFeature)
        {
            case "sw": CurrentElement.style.width = Value; break;
            case "sh": CurrentElement.style.height = Value; break;
            case "bc": CurrentElement.style.backgroundColor = Value; break;
            case "tc": CurrentElement.style.color = Value; break;
            case "fn": CurrentElement.style.fontFamily = Value; break;
            case "fs": CurrentElement.style.fontSize = Value; break;
            case "fb": CurrentElement.style.fontWeight = (Value == "1") ? "bold" : "unset"; break;
            case "vi": CurrentElement.style.visibility = (Value == "1") ? "visible" : "hidden"; break;
            case "ta": CurrentElement.style.textAlign = Value; break;
            case "sr": (Value == "1") ? CurrentElement.setAttribute("readonly", "") : CurrentElement.removeAttribute("readonly"); break;
            case "sd": (Value == "1") ? CurrentElement.setAttribute("disabled", "") : CurrentElement.removeAttribute("disabled"); break;
            case "mn": CurrentElement.setAttribute("minlength", Value); break;
            case "mx": CurrentElement.setAttribute("maxlength", Value); break;
            case "ts": CurrentElement.value = Value; break;
            case "ti":
                var SelectedIndex = parseInt(Value);
                if (SelectedIndex >= 0)
                    CurrentElement.selectedIndex = SelectedIndex;
                else
                    CurrentElement.selectedIndex = (CurrentElement.getElementsByTagName("option").length + SelectedIndex);
                break;
            case "ks":
                var CheckBoxValue = Value.GetTextBefore("|");
                var CheckBoxChecked = Value.GetTextAfter("|");
                var CheckBoxTagLength = CurrentElement.querySelectorAll('input[type="checkbox"][value="' + CheckBoxValue + '"]').length;
                if (CheckBoxTagLength > 0)
                    CurrentElement.querySelectorAll('input[type="checkbox"][value="' + CheckBoxValue + '"]')[0].checked = (CheckBoxChecked == "1");
                break;
            case "ki":
                var CheckBoxIndex = parseInt(Value.GetTextBefore("|"));
                var CheckBoxChecked = Value.GetTextAfter("|");
                var CheckBoxTags = CurrentElement.querySelectorAll('input[type="checkbox"]');
                var CheckBoxTag = (ClassIndex >= 0) ? CheckBoxTags[CheckBoxIndex] : CheckBoxTags[CheckBoxTags.length + CheckBoxIndex];
                if (CheckBoxTag)
                    CheckBoxTag.checked = (CheckBoxChecked == "1");
                break;
            case "nt":
                if (Value.Contains("|"))
                {
                    var TagName = Value.GetTextBefore("|");
                    var TagId = Value.GetTextAfter("|");
                    var TmpTag = document.createElement(TagName);
                    TmpTag.id = TagId;
                    CurrentElement.appendChild(TmpTag);
                }
                else
                    CurrentElement.appendChild(document.createElement(Value));
                break;
            case "lu": GetBack(Value, ElementPlace);
        }
    }
}

function cb_GetElementByElementPlace(ElementPlace, obj)
{
    var ElementPlaceFirstChar = ElementPlace.substring(0, 1);

    const FromPlace = (obj) ? obj : document;

    switch (ElementPlaceFirstChar)
    {
        case '<':
            var TagName = ElementPlace.substring(1).GetTextBefore(">");
            var TagIndex = (ElementPlace.length > (TagName.length + 2)) ? parseInt(ElementPlace.substring(TagName.length + 2)) : 0;
            if (TagIndex >= 0)
                return FromPlace.getElementsByTagName(TagName)[TagIndex];
            else
                return FromPlace.getElementsByTagName(TagName)[FromPlace.getElementsByTagName(TagName).length + TagIndex];

        case '(':
            var TagNameAttr = ElementPlace.substring(1).GetTextBefore(")");
            var TagNameIndex = (ElementPlace.length > (TagNameAttr.length + 2)) ? parseInt(ElementPlace.substring(TagNameAttr.length + 2)) : 0;
            if (TagNameIndex >= 0)
                return FromPlace.getElementsByName(TagNameAttr)[TagNameIndex];
            else
                return FromPlace.getElementsByName(TagNameAttr)[FromPlace.getElementsByName(TagNameAttr).length + TagNameIndex];

        case '{':
            var ClassName = ElementPlace.substring(1).GetTextBefore("}");
            var ClassIndex = (ElementPlace.length > (ClassName.length + 2)) ? parseInt(ElementPlace.substring(ClassName.length + 2)) : 0;
            if (ClassIndex >= 0)
                return FromPlace.getElementsByClassName(ClassName)[ClassIndex];
            else
                return FromPlace.getElementsByClassName(ClassName)[FromPlace.getElementsByClassName(ClassName).length + ClassIndex];

        case '*':
            var Query = ElementPlace.substring(1);
            return FromPlace.querySelector(Query.Replace("$[eq];", "="));

        case '>':
            var PlaceList = ElementPlace.substring(1).split('|');
            var TmpPlace;

            for (var i = 0; i < PlaceList.length; i++)
            {
                var TmpElementPlace = PlaceList[i];
                TmpPlace = (i == 0) ? cb_GetElementByElementPlace(TmpElementPlace) : cb_GetElementByElementPlace(TmpElementPlace, TmpPlace);
            }

            return TmpPlace;

        default: return FromPlace.getElementById(ElementPlace);
    }
}

/* End Fetch Web-Forms */

/* Start Extension Methods */

String.prototype.toDOM = function ()
{
    var DivTag = document.createElement("div");
    DivTag.innerHTML = this;

    return DivTag.innerHTML;
};

String.prototype.FullTrim = function ()
{
    return this.trim().replace(/^\s\n+|\s\n+$/g, '');
};

String.prototype.IsInput = function ()
{
    var TagName = this.toLowerCase();

    switch (TagName)
    {
        case "input":
        case "textarea":
        case "select":
        case "file":
        case "button":
            return true;
    }
    return false;
};

String.prototype.GetTextBefore = function (Text)
{
    if (!Text)
        return this;

    var index = this.indexOf(Text);
    if (index === -1)
        return "";

    return this.substring(0, index);
};

String.prototype.GetTextAfter = function (Text)
{
    if (!Text)
        return this;

    var index = this.indexOf(Text);
    if (index === -1)
        return "";

    return this.substring(index + Text.length);
};

String.prototype.DeleteHtmlClass = function(ClassName)
{
    var ClassText = this;

    if (!ClassText)
        return "";

    var ClassNameIndex = ClassText.indexOf(ClassName);

    var Space = (ClassNameIndex == 0) ? "" : " ";
        
    ClassText = ClassText.replace(Space + ClassName, "");

    if (ClassText)
        if (ClassText[0] == ' ')
            ClassText = ClassText.slice(1);

    return ClassText;
};

String.prototype.DeleteHtmlStyle = function (StyleName)
{
    var StyleText = this;
    if (!StyleText) return "";

    var StartIndex = StyleText.indexOf(StyleName);
    if (StartIndex == -1)
        return StyleText;

    var EndIndex = StartIndex + StyleName.length;
    if (StyleText[EndIndex] == ";")
        EndIndex++;

    return StyleText.substring(0, StartIndex) + StyleText.substring(EndIndex);
};

String.prototype.Contains = function (Text)
{
    return this.indexOf(Text) !== -1;
};

String.prototype.ContainsWithSpliter = function (Text, Spliter)
{
    return (Spliter + this + Spliter).indexOf(Spliter + Text + Spliter) !== -1;
};

String.prototype.ContainsNameWithSpliter = function (Text, Spliter, SpliterNameValue)
{
    return (Spliter + this).indexOf(Spliter + Text + SpliterNameValue) !== -1;
};

String.prototype.Replace = function (SearchValue, ReplaceValue)
{
    var MainText = this;
    
    if (!MainText)
        return MainText;

    if (!SearchValue)
        return MainText;

    while (MainText.indexOf(SearchValue) > -1)
        MainText = MainText.replace(SearchValue, ReplaceValue);

    return MainText;
};

String.prototype.EndsWith = function (Suffix)
{
    return this.indexOf(Suffix, this.length - Suffix.length) !== -1;
};

String.prototype.GetUnit = function ()
{
    var Value = this.toLowerCase();

    if (Value.EndsWith("%"))
        return "%";
    if (Value.EndsWith("vmax"))
        return "vmax";
    if (Value.EndsWith("vmin"))
        return "vmin";
    if (Value.EndsWith("rem"))
        return "rem";
    if (Value.EndsWith("pt"))
        return "pt";
    if (Value.EndsWith("px"))
        return "px";
    if (Value.EndsWith("em"))
        return "em";
    if (Value.EndsWith("vw"))
        return "vw";
    if (Value.EndsWith("vh"))
        return "vh";
    if (Value.EndsWith("ch"))
        return "ch";
    if (Value.EndsWith("ex"))
        return "ex";
    if (Value.EndsWith("cm"))
        return "cm";
    if (Value.EndsWith("mm"))
        return "mm";
    if (Value.EndsWith("in"))
        return "in";
    if (Value.EndsWith("pc"))
        return "pc";

    return "";
};

/* End Extension Methods */

/* Start Pre Runner Queue Methods */

function cb_SetPreRunnerQueueForEval(PreRunner, ScriptValue)
{
    if (PreRunner.length < 1)
    {
        eval(ScriptValue);
        return;
    }

    var FirstChar = PreRunner[0].substring(0, 1);

    switch (FirstChar)
    {
        case "(":
            PeriodMiliSecond = parseFloat(PreRunner[0].GetTextAfter("(")) * 1000;
            PreRunner.shift();
            setInterval(function () { cb_SetPreRunnerQueueForEval(PreRunner, ScriptValue); }, PeriodMiliSecond);
            break;
        case ":":
            DelayMiliSecond = parseFloat(PreRunner[0].GetTextAfter(":")) * 1000;
            PreRunner.shift();
            setTimeout(function () { cb_SetPreRunnerQueueForEval(PreRunner, ScriptValue); }, DelayMiliSecond);
    }
}

function cb_SetPreRunnerQueueForSetValueToInput(PreRunner, ActionOperation, ActionFeature, ActionValue)
{
    if (PreRunner.length < 1)
    {
        cb_SetValueToInput(ActionOperation, ActionFeature, ActionValue);
        return;
    }

    var FirstChar = PreRunner[0].substring(0, 1);

    switch (FirstChar)
    {
        case "(":
            PeriodMiliSecond = parseFloat(PreRunner[0].GetTextAfter("(")) * 1000;
            PreRunner.shift();
            setInterval(function () { cb_SetPreRunnerQueueForSetValueToInput(PreRunner, ActionOperation, ActionFeature, ActionValue); }, PeriodMiliSecond);
            break;
        case ":":
            DelayMiliSecond = parseFloat(PreRunner[0].GetTextAfter(":")) * 1000;
            PreRunner.shift();
            setTimeout(function () { cb_SetPreRunnerQueueForSetValueToInput(PreRunner, ActionOperation, ActionFeature, ActionValue); }, DelayMiliSecond);
    }
}

/* End Pre Runner Queue Methods */