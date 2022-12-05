from json import dumps as json

from django.http import HttpResponse


def plaintext(request):
    return HttpResponse("Hello, World!", content_type="text/plain")


def json(request):
    return HttpResponse(
            json({"message": "Hello, World!"}),
            content_type="application/json"
        )