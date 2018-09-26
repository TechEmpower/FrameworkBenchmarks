import os
import shutil


def clean(results):
    """
    Cleans the given directory of all files and folders
    """
    results_dir = os.path.dirname(results.directory)
    if os.path.exists(results_dir):
        for file in os.listdir(results_dir):
            if not os.path.exists(os.path.dirname(file)):
                shutil.rmtree(os.path.join(results_dir, file))
            else:
                os.remove(os.path.join(results_dir, file))
