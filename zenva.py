import os
import sys
import time
import subprocess
from threading import Thread
from selenium import webdriver
from selenium.webdriver.chrome.options import Options
from selenium.webdriver.common.by import By

script_dir = os.path.dirname(os.path.realpath(__file__))
cookie = open(f'{script_dir}/cookies.txt').read().strip()

dir = os.path.realpath('.')

def download_course(driver, url: str, folder_name: str):
    d = f"{dir}/{folder_name}"
    os.makedirs(d, exist_ok=True)
    os.chdir(d)
    driver.get(url)

    while True:
        tabs = driver.find_elements(By.CLASS_NAME, "course-tab")
        if len(tabs) != 5:
            time.sleep(1)
            continue
        break
    for tab in tabs:
        if tab.get_attribute("id") == "files":
            tab.click()
            time.sleep(1)
    block_files = driver.find_element(By.ID, "block-files")
    anchors = block_files.find_elements(By.TAG_NAME, "a")
    n_anchors = len(anchors)
    for i in range(n_anchors):
        anchors = block_files.find_elements(By.TAG_NAME, "a")
        anchor = anchors[i]
        href = anchor.get_attribute("href")
        if href is None:
            print("failed to get href in block_files")
            continue
        print(f"downloading {href}")
        filename = os.path.basename(href)
        anchor.click()
        dest = os.path.expanduser(f'~/Downloads/{filename}')
        if href.endswith('.txt'):
            time.sleep(3)
            content = driver.execute_script('return document.body.textContent')
            with open(dest, 'w') as f:
                f.write(content)
            driver.execute_script('window.history.go(-1)')

        while True:
            try:
                os.stat(dest)
                subprocess.call(['mv', dest, d])
                break
            except FileNotFoundError:
                time.sleep(1)

    while True:
        tabs = driver.find_elements(By.CLASS_NAME, "course-tab")
        if len(tabs) != 5:
            time.sleep(1)
            continue
        break
    for tab in tabs:
        if tab.get_attribute("id") == "curr":
            tab.click()
            time.sleep(1)
    lessons = driver.find_element(By.ID, "ul-lessons").find_elements(By.TAG_NAME, "li")
    links = []
    for lesson in lessons:
        a = lesson.find_element(By.TAG_NAME, "a")
        href = a.get_attribute("href")
        i = a.find_element(By.TAG_NAME, "i")
        icons = i.get_attribute("class")
        assert icons is not None
        if "fa-play-circle" in icons:
            texts = i.find_element(By.XPATH, "..").text
            title = texts.splitlines()[0]
            links.append((href, title))
    for (link, title) in links:
        handle_video_link(driver, link, title)
    print('finished?')

inject = """
    window.data = {}
    var originalOpen = window.XMLHttpRequest.prototype.open;
    // Redefine the 'open' method for XMLHttpRequest
    window.XMLHttpRequest.prototype.open = function() {
        if (arguments[1].endsWith('/1920x1080/video.m3u8') || arguments[1].endsWith('/1080p/video.m3u8') ) {
            console.log(arguments[1]);
            window.data.m3u8 = arguments[1];
        }

        // Call the original 'open'
        return originalOpen.apply(this, arguments);
    };
"""

def handle_video_link(driver, link: str, title: str):
    driver.get(link)
    m3u8_url = None
    for i in range(10):
        m3u8_url = driver.execute_script('return window.data.m3u8')
        if m3u8_url is not None:
            break
        time.sleep(1)
    if m3u8_url is None:
        print(f'failed to get m3u8 url for link {link}')
        return
    download_video(m3u8_url, title)


def download_video(m3u8_url, title):
    subprocess.call(['bash', os.path.expanduser('~/misc/m3u8_tool/mk_from_url.sh'), m3u8_url, title])

def main():
    url = sys.argv[1]
    if url[len(url) - 1] == '/':
        url = url[:-1]
    title = os.path.basename(url)

    chrome_options = Options()
    chrome_options.add_experimental_option("debuggerAddress", "127.0.0.1:9222")
    driver = webdriver.Chrome(options=chrome_options)

    driver.execute_cdp_cmd('Page.addScriptToEvaluateOnNewDocument', {'source': inject})
    download_course(driver, sys.argv[1], title)

if __name__ == "__main__":
    main()
