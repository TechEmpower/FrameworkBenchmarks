# play2 framework tests

1. Add new test applications in subdirectories named either `play2-language` or `play2-language-orm`.
2. Edit `generate_config.py` and add configuration for the new test applications.
3. Run `python generate_config.py` to generate a new `benchmark_config` file and to generate a `setup_play2_*.py` file for your test application.