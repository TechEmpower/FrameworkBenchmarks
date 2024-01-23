import jinja2
from pathlib import Path
from collections import namedtuple
import random


def sanitize(query):
    if not query.isnumeric():
        res = 1
    else:
        query = int(query)
        if query < 1:
            res = 1
        elif query > 500:
            res = 500
        else:
            res = query
    return res


def generate_ids(num_queries):
    return random.sample(range(1, 10000), num_queries)


def load_template():
    path = Path("templates", "fortune.html")
    with open(str(path), "r") as template_file:
        template_text = template_file.read()
        return jinja2.Template(template_text)


FortuneTuple = namedtuple("Fortune", ["id", "message"])
