import logging
import asyncio
import ujson

from ..services import queries_number
from ..services.world import get_random_record, get_random_records, update_random_records, get_fortunes

LOG = logging.getLogger(__name__)

@asyncio.coroutine
def db(request):
    """Test type 2: Single database query"""
    container = request.app.ah_container

    return ujson.dumps((yield from get_random_record(container)))

@asyncio.coroutine
def queries(request):
    """Test type 3: Multiple database queries"""
    container = request.app.ah_container
    limit = queries_number(request.params.get('queries', 1))

    return ujson.dumps((yield from get_random_records(container, limit)))

@asyncio.coroutine
def fortunes(request):
    """Test type 4: Fortunes"""
    container = request.app.ah_container
    template = request.app['j2_env'].get_template('fortunes.html.j2')
    return template.render({'fortunes': (yield from get_fortunes(container))})

@asyncio.coroutine
def updates(request):
    """Test type 5: Database updates"""
    container = request.app.ah_container
    limit = queries_number(request.params.get('queries', 1))

    return ujson.dumps((yield from update_random_records(container, limit)))