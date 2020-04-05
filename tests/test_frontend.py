"Frontend tests using Selenium."

from selenium.webdriver.support.ui import WebDriverWait


def test_index(service_endpoint, webdriver):
    "Index should be available."

    webdriver.get(f"{service_endpoint}/")

    WebDriverWait(webdriver, 3).until(lambda d: "Login" in d.title)
    assert "Login" in webdriver.page_source
