-module(chicagoboss_outgoing_mail_controller).
-compile(export_all).

%% See http://www.chicagoboss.org/api-mail-controller.html for what should go in here

test_message(FromAddress, ToAddress, Subject) ->
    Headers = [
        {"Subject", Subject},
        {"To", ToAddress},
        {"From", FromAddress}
    ],
    {ok, FromAddress, ToAddress, Headers, [{address, ToAddress}]}.
