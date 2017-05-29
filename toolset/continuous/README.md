# TFB Continuous Benchmarking

Performs continuous benchmarking of all frameworks and uploads the results to `TFB_UPLOADURI` found in `tfb.conf`.

### Setup

* Move the `tfb.conf` file to `/etc/init`
* `sudo service tfb start`

### Notes

If `run-continuously.sh` is modified the tfb service will need to be restarted. Alternatively, any new or modified tasks in `run-tasks.sh` will now be updated automatically for each continuous run.
