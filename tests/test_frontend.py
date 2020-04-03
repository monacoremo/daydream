'''
Frontend tests.

'''

import requests
import pytest
import selenium.webdriver
from selenium.webdriver.support.ui import WebDriverWait


@pytest.yield_fixture
def webdriver():
    options = selenium.webdriver.FirefoxOptions()
    options.headless = True
    driver = selenium.webdriver.Firefox(options=options)
    yield driver
    driver.quit()


def test_index(service_endpoint, webdriver):
    'Index should be available.'
    webdriver.get(f'{service_endpoint}/')
    WebDriverWait(webdriver, 10).until(lambda d: 'stack' in d.title)
    assert 'Full' in webdriver.title
