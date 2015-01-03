import logging
import asyncio

from aiohttp.web import Response

from ..utils import JSON
from ..services import queries_number
from ..services.world import get_random_record, get_random_records, update_random_records

LOG = logging.getLogger(__name__)

@asyncio.coroutine
def json(request):
    """Test type 1: JSON serialization"""
    return JSON({'message': 'Hello, World!'})

@asyncio.coroutine
def db(request):
    """Test type 2: Single database query"""
    container = request.app.ah_container

    return JSON((yield from get_random_record(container)))

@asyncio.coroutine
def queries(request):
    """Test type 3: Multiple database queries"""
    container = request.app.ah_container
    limit = queries_number(request.GET.get('queries', 1))

    return JSON((yield from get_random_records(container, limit)))

@asyncio.coroutine
def updates(request):
    """Test type 5: Database updates"""
    container = request.app.ah_container
    limit = queries_number(request.GET.get('queries', 1))

    return JSON((yield from update_random_records(container, limit)))

@asyncio.coroutine
def plaintext(request):
    """Test type 6: Plaintext"""
    return Response(text='Hello, World!')