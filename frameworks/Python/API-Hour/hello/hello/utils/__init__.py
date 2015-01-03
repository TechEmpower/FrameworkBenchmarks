import logging

from aiohttp.web import Response
import ujson

LOG = logging.getLogger(__name__)

class JSON(Response):
    """Serialize response to JSON"""

    def __init__(self, json, status=200,
                 reason=None, headers=None):
        body = ujson.dumps(json).encode('utf-8')

        super().__init__(body=body, status=status, reason=reason,
                         headers=headers, content_type='application/json')
