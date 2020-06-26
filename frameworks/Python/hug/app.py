import hug

from datetime import datetime


# Create decorators for mimetypes (JSON is default)
plaintext = hug.get(output=hug.output_format.text)
json      = hug.get(output=hug.output_format.json)


# Create a directive to add necessary headers
@hug.response_middleware()
def set_required_headers(request, response, resource):
    date_obj      = datetime.now()
    rfc_1123      = "%a, %d %b %Y %H:%M:%S GMT"
    rfc_1123_date = date_obj.strftime(rfc_1123)

    headers       = { "Server": "hug", "Date": rfc_1123_date }

    response.set_headers(headers)


@plaintext
@hug.get("/plaintext")
def plaintext():
    """Plaintext handler."""
    return "Hello, World!"

app = hug.API(__name__).http.server()
