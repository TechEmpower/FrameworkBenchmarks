:- web_resource(plaintext/1, content_type(text/plain)).
plaintext('Hello, World!').

:- web_resource(json/1).
json([message('Hello, World!')]).
