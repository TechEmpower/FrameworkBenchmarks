import logging
import asyncio
from random import randint

from ..services import queries_number
from ..services.world import get_random_record, get_random_records

LOG = logging.getLogger(__name__)

@asyncio.coroutine
def json(request):
    """Test type 1: JSON serialization"""
    return {'message': 'Hello, World!'}

@asyncio.coroutine
def db(request):
    """Test type 2: Single database query"""
    container = request.application.ah_container

    return (yield from get_random_record(container))

@asyncio.coroutine
def queries(request):
    """Test type 3: Multiple database queries"""
    container = request.application.ah_container
    limit = queries_number(request.args.get('queries', 1))

    return (yield from get_random_records(container, limit))