'''
Frontend tests.

'''

import os
import subprocess
import time

import requests
import pytest
import selenium.webdriver
from selenium.webdriver.support.ui import WebDriverWait


BASE_URL = os.environ.get('TESTS_BASE_URI')


@pytest.yield_fixture
def webdriver():
    options = selenium.webdriver.FirefoxOptions()
    options.headless = True
    driver = selenium.webdriver.Firefox(options=options)
    yield driver
    driver.quit()


def test_index(webdriver):
    'Index should be available.'
    webdriver.get(f'{BASE_URL}/')
    WebDriverWait(webdriver, 10).until(lambda d: 'stack' in d.title)
    assert 'Full' in webdriver.title
