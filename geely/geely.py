#!/usr/bin/env python

import base64
import os
from selenium import webdriver
from selenium.webdriver.chrome.options import Options
from selenium.webdriver.chrome.service import Service
import time
import json

FOLDER = './data/'

MANUAL_JSON = os.path.join(os.path.dirname(__file__), 'manual.json')
BASE_URL = 'https://gtis.geely.com/gum/manual/html/KX11-KX11A3/topics/'

def init_chrome_driver(download_dir=None):
    """
    Initialize a headless Chrome WebDriver with print-to-PDF capability.
    Optionally set the download directory for PDFs.
    """
    options = Options()
    options.add_argument('--headless=new')
    options.add_argument('--disable-gpu')
    options.add_argument('--no-sandbox')
    prefs = {}
    if download_dir:
        os.makedirs(download_dir, exist_ok=True)
        prefs['download.default_directory'] = download_dir
        prefs['savefile.default_directory'] = download_dir
        prefs['printing.print_preview_sticky_settings.appState'] = '{"recentDestinations": [{"id": "Save as PDF", "origin": "local", "account": ""}], "selectedDestinationId": "Save as PDF", "version": 2}'
        prefs['printing.default_destination_selection_rules'] = '{"kind": "local", "namePattern": ".*PDF.*"}'
        prefs['profile.default_content_settings.popups'] = 0
        prefs['download.prompt_for_download'] = False
        prefs['plugins.always_open_pdf_externally'] = True
    if prefs:
        options.add_experimental_option('prefs', prefs)
    options.add_argument('--kiosk-printing')
    service = Service(executable_path='/usr/bin/chromedriver')
    driver = webdriver.Chrome(options=options, service=service)
    return driver

def save_link_as_pdf(driver, url, output_pdf_path):
    """
    Open the given URL in Chrome and save the page as a PDF to output_pdf_path.
    """
    driver.get(url)
    time.sleep(2)  # Wait for page to load; adjust as needed
    # Use Chrome DevTools Protocol to print to PDF
    pdf = driver.execute_cdp_cmd("Page.printToPDF", {"printBackground": True})
    with open(output_pdf_path, "wb") as f:
        f.write(base64.b64decode(pdf['data']))
    time.sleep(1)

# Recursively traverse the catalog and save PDFs for each leaf node

def traverse_and_save(driver, node, path_parts):
    label = node.get('label', '')
    if label:
        # Replace '/' with '_' in label for file/folder names
        safe_label = label.replace('/', '_')
        path_parts = path_parts + [safe_label]
    if 'code' in node:
        code = node['code']
        url = f"{BASE_URL}{code}.html"
        # Build output path: FOLDER/label1/label2/.../labelN.pdf
        out_dir = os.path.join(FOLDER, *path_parts[:-1])
        os.makedirs(out_dir, exist_ok=True)
        out_path = os.path.join(out_dir, f"{path_parts[-1]}.pdf")
        if os.path.exists(out_path):
            print(f"Skipping existing file: {out_path}")
            return
        print(f"Saving {url} -> {out_path}")
        save_link_as_pdf(driver, url, out_path)
    if 'children' in node:
        for child in node['children']:
            traverse_and_save(driver, child, path_parts)

def main():
    with open(MANUAL_JSON, 'r', encoding='utf-8') as f:
        manual = json.load(f)
    driver = init_chrome_driver(download_dir=FOLDER)
    for top in manual['catalog']:
        traverse_and_save(driver, top, [])
    driver.quit()

if __name__ == '__main__':
    main()
