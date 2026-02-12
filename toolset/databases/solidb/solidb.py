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
    def _get_http_requests_total(cls, config):
        """Parse Prometheus /metrics endpoint for solidb_http_requests_total."""
        try:
            base = cls._base_url(config)
            resp = requests.get("%s/metrics" % base)
            total = 0
            for line in resp.text.splitlines():
                if line.startswith("#"):
                    continue
                if line.startswith("solidb_http_requests_total"):
                    parts = line.split()
                    if len(parts) >= 2:
                        total += int(float(parts[-1]))
            return total
        except Exception:
            return 0

    @classmethod
    def _get_rows_per_query(cls, config):
        if cls.tbl_name == "fortune":
            try:
                base = cls._base_url(config)
                headers = cls._auth_headers(config)
                resp = requests.post(
                    "%s/_api/database/hello_world/query" % base,
                    headers=headers,
                    json={"query": "FOR f IN fortunes RETURN f"})
                return len(resp.json().get("result", []))
            except Exception:
                return 12
        return 1

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
                "%s/_api/database/hello_world/query" % base,
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
        return cls._get_http_requests_total(config)

    @classmethod
    def get_rows(cls, config):
        return cls._get_http_requests_total(config) * cls._get_rows_per_query(config)

    @classmethod
    def get_rows_updated(cls, config):
        return cls._get_http_requests_total(config)

    @classmethod
    def reset_cache(cls, config):
        return
