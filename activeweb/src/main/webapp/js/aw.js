/*
 Copyright 2009-2010 Igor Polevoy

 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

 http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
 */

/*
 This file is a collection of unobtrusive JS that binds to link_to generated anchors typical for Ajax calls.

 author: Igor Polevoy
 */

$(document).ready(function() {
    $('a[data-link]').bind('click', function() {
        var anchor = $(this);
        var destination = anchor.attr("data-destination");
        var formId = anchor.attr("data-form");
        var href = anchor.attr("href");
        var _method = anchor.attr("data-method");
        var before = anchor.attr("data-before");
        var after = anchor.attr("data-after");
        var beforeArg = anchor.attr("data-before-arg");
        var afterArg = anchor.attr("data-after-arg");
        var error = anchor.attr("data-error");

        var confirmMessage = anchor.attr("data-confirm");

        if(confirmMessage != null ){
            if(!confirm(confirmMessage)){
                return false;
            }
        }

        //not Ajax
        if(destination == null && before == null && after == null && (_method == null || _method.toLowerCase() == "get")){
            return true;
        }

        if (_method == null) {
            _method = "get";
        }
        var type;
        if (_method.toLowerCase() == "get") {
            type = "get";
        } else if (_method.toLowerCase() == "post"
                || _method.toLowerCase() == "put"
                || _method.toLowerCase() == "delete") {
            type = "post";
        }

        var data = "_method=" + _method;
        if (formId != null) {
            data += "&" + $("#" + formId).serialize();
        }

        if(before != null){
            eval(before)(beforeArg);
        }


        $.ajax({ url: href, data: data, type:type,
            success: function(data) {
                if (after != null)
                    eval(after)(afterArg, data);

                if (destination != null)
                    $("#" + destination).html(data);
            },
            error: function(xhr, status, errorThrown) {
                if(error != null){
                    eval(error)(xhr.status, xhr.responseText );
                }
            }
        });

        return false;
    });
});
