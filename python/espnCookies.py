
from selenium import webdriver
from selenium.webdriver.support.events import EventFiringWebDriver, AbstractEventListener

class Listener(AbstractEventListener):
    def after_navigate_to(self, url, driver):
        print("After navigate to %s" % url)

edriver = EventFiringWebDriver(webdriver.Chrome(), Listener())
edriver.get("https://google.com")
