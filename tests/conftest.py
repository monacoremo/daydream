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


SERVICE_BIN = os.environ['TESTS_SERVICE_BIN']
SERVICE_ENDPOINT = os.environ['TESTS_SERVICE_URI']



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
    'Fixture for a selenium webdriver'

    options = selenium.webdriver.FirefoxOptions()
    options.headless = True
    driver = selenium.webdriver.Firefox(options=options)

    try:
        yield driver
    finally:
        driver.quit()



# ACCOUNTS AND SESSIONS


@pytest.fixture(scope='session')
def alice_email():
    'Fixture for a user email.'
    return f'alice-{time.time()}@test.org'


@pytest.fixture(scope='session')
def alice_password():
    'Fixture for a user password.'
    return 'alicesecret'


@pytest.fixture(scope='session')
def alice_account(service_endpoint, alice_email, alice_password):
    'Fixture for a user account.'
    session = requests.Session()
    resp = session.post(f'{service_endpoint}/api/rpc/register', json={
        'email': alice_email,
        'name': 'Alice',
        'password': alice_password,
    })

    assert resp.status_code == 200


@pytest.fixture()
def alice_session(service_endpoint, alice_email, alice_account, alice_password):
    'Fixture for a logged in web session'
    session = requests.Session()
    resp = session.post(f'{service_endpoint}/api/rpc/login', json={
        'email': alice_email,
        'password': 'alicesecret',
    })

    assert resp.status_code == 200

    return session


@pytest.fixture(scope='session')
def bob_email():
    'Fixture for a user email.'
    return f'bob-{time.time()}@test.org'


@pytest.fixture(scope='session')
def bob_password():
    'Fixture for a user password.'
    return 'bobsecret'


@pytest.fixture(scope='session')
def bob_account(service_endpoint, bob_email, bob_password):
    'Fixture for a user account.'
    session = requests.Session()
    resp = session.post(f'{service_endpoint}/api/rpc/register', json={
        'email': bob_email,
        'name': 'Bob',
        'password': bob_password,
    })

    assert resp.status_code == 200


@pytest.fixture()
def bob_session(service_endpoint, bob_email, bob_account, bob_password):
    'Fixture for a logged in web session'
    session = requests.Session()
    resp = session.post(f'{service_endpoint}/api/rpc/login', json={
        'email': bob_email,
        'password': bob_password,
    })

    assert resp.status_code == 200

    return session
