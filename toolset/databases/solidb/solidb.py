import json
import traceback

import requests
from colorama import Fore
from toolset.utils.output_helper import log
from toolset.databases.abstract_database import AbstractDatabase


class Database(AbstractDatabase):

    _token = None

    @classmethod
    def _base_url(cls, config):
        return "http://%s:6745" % config.database_host

    @classmethod
    def _get_token(cls, config):
        if cls._token is None:
            base = cls._base_url(config)
            resp = requests.post(
                "%s/auth/login" % base,
                json={"username": "admin", "password": "benchmarkdbpass"})
            cls._token = resp.json().get("token")
        return cls._token

    @classmethod
    def _auth_headers(cls, config):
        token = cls._get_token(config)
        return {
            "Content-Type": "application/json",
            "Authorization": "Bearer %s" % token
        }

    @classmethod
    def get_connection(cls, config):
        session = requests.Session()
        session.headers.update(cls._auth_headers(config))
        return session

    @classmethod
    def get_current_world_table(cls, config):
        results_json = []

        try:
            base = cls._base_url(config)
            headers = cls._auth_headers(config)
            worlds_json = {}
            print("DATABASE_HOST: %s" % config.database_host)
            resp = requests.post(
                "%s/_api/sdbql/hello_world" % base,
                headers=headers,
                json={"query": "FOR w IN worlds RETURN w"}
            )
            for world in resp.json().get("result", []):
                if "randomNumber" in world:
                    worlds_json[str(int(world["id"]))] = int(
                        world["randomNumber"])
            results_json.append(worlds_json)
        except Exception:
            tb = traceback.format_exc()
            log("ERROR: Unable to load current SoliDB World table.",
                color=Fore.RED)
            log(tb)

        return results_json

    @classmethod
    def test_connection(cls, config):
        try:
            base = cls._base_url(config)
            resp = requests.get("%s/_api/health" % base)
            return resp.status_code == 200
        except Exception:
            return False

    @classmethod
    def get_queries(cls, config):
        try:
            base = cls._base_url(config)
            headers = cls._auth_headers(config)
            resp = requests.get("%s/_api/stats" % base, headers=headers)
            stats = resp.json()
            return int(stats.get("queries", 0))
        except Exception:
            return 0

    @classmethod
    def get_rows(cls, config):
        try:
            base = cls._base_url(config)
            headers = cls._auth_headers(config)
            resp = requests.get("%s/_api/stats" % base, headers=headers)
            stats = resp.json()
            return int(stats.get("rows_read", 0))
        except Exception:
            return 0

    @classmethod
    def get_rows_updated(cls, config):
        try:
            base = cls._base_url(config)
            headers = cls._auth_headers(config)
            resp = requests.get("%s/_api/stats" % base, headers=headers)
            stats = resp.json()
            return int(stats.get("rows_updated", 0))
        except Exception:
            return 0

    @classmethod
    def reset_cache(cls, config):
        return
