#!/usr/bin/env python3
import os
import subprocess
import sys

def main() -> int:
    # Minimal helper for manual use; not required by current TFB toolset.
    cmd = os.environ.get("TFB_COMMAND", "/app/server")
    if os.path.exists(cmd) and os.access(cmd, os.X_OK):
        os.execv(cmd, [cmd])
    return subprocess.call([
        "cargo",
        "run",
        "--release",
        "--features",
        "hotaru_server",
    ])

if __name__ == "__main__":
    raise SystemExit(main())
