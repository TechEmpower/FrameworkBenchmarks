import os
import json


for lang in os.listdir('frameworks'):
    for fw in os.listdir(os.path.join('frameworks', lang)):
        config = os.path.join('frameworks', lang, fw, 'benchmark_config.json')
        new_config = os.path.join('frameworks', lang, fw, 'config.toml')
        if os.path.exists(config):
            with open(config, "r") as f:
                data = json.loads(f.read())
                with open(new_config, "w") as n:
                    n.write('[framework]\n')
                    n.write('name = "{}"\n'.format(data['framework']))

                    for test in data['tests']:
                        for key in test:
                            n.write('\n[{}]\n'.format(key if key != 'default' else 'main'))
                            if 'plaintext_url' in test[key]:
                                n.write('urls.plaintext = "{}"\n'.format(test[key]['plaintext_url']))
                            if 'json_url' in test[key]:
                                n.write('urls.json = "{}"\n'.format(test[key]['json_url']))
                            if 'db_url' in test[key]:
                                n.write('urls.db = "{}"\n'.format(test[key]['db_url']))
                            if 'query_url' in test[key]:
                                n.write('urls.query = "{}"\n'.format(test[key]['query_url']))
                            if 'update_url' in test[key]:
                                n.write('urls.update = "{}"\n'.format(test[key]['update_url']))
                            if 'fortune_url' in test[key]:
                                n.write('urls.fortune = "{}"\n'.format(test[key]['fortune_url']))
                            if 'cached_query_url' in test[key]:
                                n.write('urls.cached_query = "{}"\n'.format(test[key]['cached_query_url']))
                            if 'name' in test[key]:
                                n.write('name = "{}"\n'.format(test[key]['name']))
                            if 'approach' in test[key]:
                                n.write('approach = "{}"\n'.format(test[key]['approach']))
                            if 'classification' in test[key]:
                                n.write('classification = "{}"\n'.format(test[key]['classification']))
                            if 'database' in test[key]:
                                n.write('database = "{}"\n'.format(test[key]['database']))
                            if 'database_os' in test[key]:
                                n.write('database_os = "{}"\n'.format(test[key]['database_os']))
                            if 'os' in test[key]:
                                n.write('os = "{}"\n'.format(test[key]['os']))
                            if 'orm' in test[key]:
                                n.write('orm = "{}"\n'.format(test[key]['orm']))
                            if 'platform' in test[key]:
                                n.write('platform = "{}"\n'.format(test[key]['platform']))
                            if 'webserver' in test[key]:
                                n.write('webserver = "{}"\n'.format(test[key]['webserver']))
                            if 'versus' in test[key]:
                                n.write('versus = "{}"\n'.format(test[key]['versus']))
                            if 'dockerfile' in test[key]:
                                n.write('dockerfile = "{}"\n'.format(test[key]['dockerfile']))
