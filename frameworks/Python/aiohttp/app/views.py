from aiohttp.web import Response
import ujson


async def json(request):
    """
    Test 1
    """
    body = ujson.dumps({'message': 'Hello, World!'})
    return Response(body=body.encode(), content_type='application/json')


async def plaintext(request):
    """
    Test 6
    """
    return Response(body=b'Hello, World!', content_type='text/plain')
