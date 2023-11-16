"""
TFB Fail Detector Web Scraper
Scrapes data from the last 3 complete runs and outputs the frameworks that have failed all of them with the same errors.

Developed using selenium==4.10.0 and the selenium/standalone-chrome:4.10.0 docker image.

USAGE: python tfb-fail-detector.py
"""

from collections import defaultdict
import re

from selenium import webdriver
from selenium.webdriver.chrome.options import Options
from selenium.webdriver.common.by import By

NUM_RUNS = 3

def get_driver():
  """Gets the selenium webdriver for interacting with the website"""
  chrome_options = Options()
  chrome_options.add_argument("--headless")
  chrome_options.add_argument("--no-sandbox")
  chrome_options.add_argument("--disable-dev-shm-usage")
  chrome_prefs = {}
  chrome_options.experimental_options["prefs"] = chrome_prefs
  chrome_prefs["profile.default_content_settings"] = {"images": 2}
  driver = webdriver.Chrome(options=chrome_options)
  return driver

def get_last_n_complete_runs(driver, n=3):
  """Scrape the last n complete runs"""
  rows = driver.find_elements(By.CSS_SELECTOR, "table.resultsTable > tbody > tr")
  complete_runs = []
  for row in rows:
    row_uuid = row.get_dom_attribute("data-uuid")
    run_stats = row.find_element(By.CSS_SELECTOR, "td:nth-of-type(2)")
    frameworks_tested_stats = re.search(r"([0-9]+)/([0-9]+) frameworks tested", run_stats.text)
    if not frameworks_tested_stats:
      # print("Unable to get info from run %s" % row_uuid)
      continue
    tested, total = frameworks_tested_stats.groups()
    if tested != total:
      # print("Found incomplete run %s. Tested: %s/%s" % (row_uuid, tested, total))
      continue
    # print("Found complete run %s. Tested: %s/%s" % (row_uuid, tested, total))
    complete_runs.append(row_uuid)
    if len(complete_runs) >= n:
      return complete_runs

def find_failing_frameworks(driver, run_uuids):
  """Find frameworks that have failed all the given runs with the same error message"""
  failing_frameworks = defaultdict(lambda: 0)
  def process_failed_framework(framework_element):
    framework = re.search(r"\[\S+] [a-zA-Z][a-zA-Z:\_ ]*$", framework_element.text)
    framework_failure = framework.group(0)
    failing_frameworks[framework_failure] += 1

  for run_uuid in run_uuids:
    driver.get("https://tfb-status.techempower.com/results/%s" % run_uuid)
    assert "TFB Status" in driver.title
    failure_list = driver.find_element(By.CLASS_NAME, "failures")
    failures = failure_list.find_elements(By.TAG_NAME, "li")
    for failure in failures:
      process_failed_framework(failure)
  return failing_frameworks

if __name__ == '__main__':
  driver = get_driver()
  driver.get("https://tfb-status.techempower.com/")
  assert "TFB Status" in driver.title
  complete_runs = get_last_n_complete_runs(driver, NUM_RUNS)
  failed_frameworks = find_failing_frameworks(driver, complete_runs)

  for failure_info, fail_count in failed_frameworks.items():
    if fail_count != NUM_RUNS:
      continue
    print(failure_info)
