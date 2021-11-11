#!/usr/bin/env python

from __future__ import print_function

import json

from spyne import AnyUri, Unicode, ComplexModel, M, UnsignedInteger16, Array
from spyne.protocol.json import JsonDocument
from spyne.util.dictdoc import get_object_as_dict


class BenchmarkConfigElement(ComplexModel):
    # exclude this from the output document
    key = Unicode(pa={JsonDocument: dict(exc=True)})

    display_name = M(Unicode)
    notes = Unicode
    versus = Unicode

    db_url = AnyUri
    json_url = AnyUri
    query_url = AnyUri
    fortune_url = AnyUri
    update_url = AnyUri
    plaintext_url = AnyUri

    port = M(UnsignedInteger16(default=8080))

    approach = M(Unicode(values=['Realistic', 'Stripped'], default='Realistic'))
    classification = M(Unicode(values=['Micro', 'Fullstack', 'Platform'], default='Micro'))
    database = M(Unicode(values=['none', 'mongodb', 'postgres', 'mysql'], default='none'))
    orm = M(Unicode(values=['Full', 'Micro', 'None', 'Raw']))

    framework = M(Unicode)
    language = M(Unicode)
    flavor = M(Unicode)
    platform = M(Unicode)
    webserver = M(Unicode)

    os = M(Unicode(default='Linux'))
    database_os = M(Unicode(default='Linux'))
    
    
class BenchmarkConfig(ComplexModel):
    framework = M(Unicode)
    tests = Array(BenchmarkConfigElement, wrapped=False)


gen_raw_test = lambda: BenchmarkConfigElement(
    display_name="Spyne RAW",
    db_url="/dbsraw",
    query_url="/dbraw?queries=",
    fortune_url="/fortunesraw",
    update_url="/raw-updates?queries=",
    orm='Raw',
)

gen_normal_test = lambda: BenchmarkConfigElement(
    display_name="Spyne ORM",
    db_url="/dbs",
    query_url="/db?queries=",
    fortune_url="/fortunes",
    update_url="/updatesraw?queries=",
    orm='Full',
)


def add_common(bc):
    bc.port = 8080
    bc.approach = "Realistic"
    bc.classification = "Micro"
    bc.database = "postgres"
    bc.framework = "spyne"
    bc.language = "Python"
    bc.platform = "Spyne"
    bc.webserver = "None"
    bc.os = "Linux"
    bc.database_os = "Linux"
    bc.versus = "wsgi"
    bc.plaintext_url = "/plaintext"
    return bc


config = BenchmarkConfig(framework='spyne', tests=[])

keys = iter(['default', 'raw', 'py3orm', 'py3raw'])

for flav in ['CPython', 'Python3']:
    bc = add_common(gen_normal_test())
    bc.flavor = flav
    bc.key = next(keys)
    config.tests.append(bc)

    bc = add_common(gen_raw_test())
    bc.flavor = flav
    bc.key = next(keys)
    config.tests.append(bc)

data = get_object_as_dict(config, complex_as=dict)
data['tests'] = [{d['key']: d} for d in data['tests']]

data = json.dumps(data, indent=2, sort_keys=True, separators=(',', ': '))

open('benchmark_config.json', 'wb').write(data)


print(data)
