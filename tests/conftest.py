import pytest
import subprocess
import os
import time
import sys
import signal
from contextlib import contextmanager

import requests
from requests.exceptions import Timeout, ConnectionError
import selenium.webdriver


SERVICE_BIN = os.environ.get('TESTS_SERVICE_BIN')
SERVICE_ENDPOINT = os.environ.get('TESTS_SERVICE_URI')


# SERVICE

@pytest.fixture(scope='session')
def service_endpoint():
    '''Service endpoint that is ready for requests.'''

    with service_process():
        retry_until_ok(f'{SERVICE_ENDPOINT}/healthcheck')
        yield SERVICE_ENDPOINT


@contextmanager
def service_process():
    '''Spin up and terminate the service.'''

    # spawn process with a new process group, so that it can be terminated by
    # itself
    with subprocess.Popen(SERVICE_BIN, preexec_fn=os.setsid) as process:
        try:
            yield process
        finally:
            process.terminate()


def retry_until_ok(url, retries=100):
    '''Retry a URL with HTTP GET requests until it returns '200 OK'.'''

    for _ in range(retries):
        try:
            response = requests.get(url, timeout=1)

            if response.status_code == 200:
                return
            else:
                time.sleep(0.1)
        except ConnectionError:
            time.sleep(0.1)
        except Timeout:
            pass

    raise Timeout()


# SELENIUM

@pytest.fixture(scope='session')
def webdriver():
    options = selenium.webdriver.FirefoxOptions()
    options.headless = True
    driver = selenium.webdriver.Firefox(options=options)

    try:
        yield driver
    finally:
        driver.quit()
