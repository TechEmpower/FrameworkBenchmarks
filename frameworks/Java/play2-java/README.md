# play2-java framework tests

1. Add new test applications in subdirectories named either `play2-java` or `play2-java-orm`.
2. Edit `frameworks/Java/play2-java/generate_config.py` and add configuration for the new test applications.
3. Run `python frameworks/Java/play2-java/generate_config.py` to generate a new `benchmark_config.json` file and to generate a `setup_*.py` file for your test application.