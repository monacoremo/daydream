'''
Frontend tests using Selenium.

'''

from selenium.webdriver.support.ui import WebDriverWait


def test_index(service_endpoint, webdriver):
    'Index should be available.'
    webdriver.get(f'{service_endpoint}/')

    WebDriverWait(webdriver, 10).until(lambda d: 'stack' in d.title)
    assert 'Full' in webdriver.title
