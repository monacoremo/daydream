"Frontend tests using Selenium."

from selenium.webdriver.support.ui import WebDriverWait


def test_index(service_endpoint, selenium_session):
    "Index should be available."

    with selenium_session() as webdriver:
        webdriver.get(f"{service_endpoint}/")

        WebDriverWait(webdriver, 10).until(lambda d: "Login" in d.title)
        assert "Login" in webdriver.page_source

        webdriver.save_screenshot("01_login_screen.png")

        email_input = webdriver.find_element_by_xpath(
            "//label[contains(.,'Email')]//input"
        )
        email_input.clear()
        email_input.send_keys("alice@test.org")

        password_input = webdriver.find_element_by_xpath(
            "//label[contains(.,'Password')]//input"
        )
        password_input.clear()
        password_input.send_keys("alicesecret")

        webdriver.save_screenshot("02_filled_out.png")

        submit_button = webdriver.find_element_by_xpath(
            "//div[contains(.,'Login')][@role='button']"
        )
        submit_button.click()

        webdriver.save_screenshot("03_submitted.png")

        WebDriverWait(webdriver, 10).until(lambda d: "Logged in" in d.title)
        webdriver.save_screenshot("04_index_screen.png")
