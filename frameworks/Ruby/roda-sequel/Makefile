json: benchmark_config.yaml
	ruby -r json -r yaml -e 'puts JSON.pretty_generate(YAML.load(ARGF.read))' <benchmark_config.yaml >benchmark_config.json
