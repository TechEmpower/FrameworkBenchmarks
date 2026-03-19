from web_framework_api import *


def on_start():
    print("Server is running...")


if __name__ == '__main__':
    initialize_web_framework()

    server = WebFramework("config.json")

    server.start(True, on_start)
