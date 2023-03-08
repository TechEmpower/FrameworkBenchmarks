from json import dumps

from django.http import HttpResponse


def plaintext(request):
    return HttpResponse("Hello, World!", content_type="text/plain")


def json(request):
    return HttpResponse(
            dumps({"message": "Hello, World!"}),
            content_type="application/json"
        )