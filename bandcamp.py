#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.12"
# dependencies = [
# "requests",
# "playwright",
# ]
# ///

import os
import re
import time
import json

import requests
from playwright.sync_api import sync_playwright, Playwright

STATE_FILE = 'auth_state.json'
STOP_FILE = ".stop"
SAVE_FILE = ".save"

os.system('which playwright')
# breakpoint()

# prepare state file
def run_preamble(playwright: Playwright):
    browser = playwright.firefox.launch(headless=False)
    context = browser.new_context()
    page = context.new_page()
    page.goto("https://bandcamp.com/c4r50nz")
    print('waiting for login...')
    while True:
        time.sleep(1)
        if os.path.exists(SAVE_FILE):
            os.unlink(SAVE_FILE)
            context.storage_state(path=STATE_FILE)
            break

GREEN = '\033[92m'
YELLOW = '\033[93m'
END = '\033[0m'

def run_first_stage(playwright: Playwright):
    browser = playwright.firefox.launch(headless=False)

    context = browser.new_context(storage_state=STATE_FILE)

    page = context.new_page()

    page.goto("https://bandcamp.com/c4r50nz")
    print("Navigated to the page...")

    expand = page.get_by_text(re.compile(r"view all \d+ items"))
    text = expand.text_content()
    match = re.match(r"view all (\d+) items", text)
    total_items = int(match.group(1))
    print(f"Total items: {total_items}")
    expand.click()

    while True:
        time.sleep(1)
        page.mouse.wheel(0, 1000)
        downloads = page.locator('li.collection-item-container').all()

        if len(downloads) == total_items:
            print("All items appears")
            break
    links = []
    for download in downloads:
        title = download.locator('div.collection-item-title').last.text_content()
        artist = download.locator('div.collection-item-artist').last.text_content()
        if artist.startswith('by '):
            artist = artist[3:]
        link = download.locator('span.redownload-item', has_text='download'). \
                locator('a').get_attribute('href')
        links.append({
            'title': title,
            'artist': artist,
            'link': link
        })
    with open('links.json', 'w') as f:
        json.dump(links, f, indent=2)
        print(f"Total links saved: {len(links)}")

    while True:
        if os.path.exists(STOP_FILE):
            os.unlink(STOP_FILE)
            break
        time.sleep(2)

def compose_zip_filename(artist: str, title: str):
    return f"{artist} - {title}.zip".replace(':', '-')

def run_second_stage(playwright: Playwright, output_folder: str, prefix=""):
    browser = playwright.firefox.launch(headless=False)
    context = browser.new_context(storage_state=STATE_FILE)
    page = context.new_page()

    with open('links.json', 'r') as f:
        links = json.load(f)
    print(f"Total links: {len(links)}")

    # Some spacing around '-' is not preserved in the file name
    done_or_downloading = set([f for f in os.listdir(output_folder) if f.endswith('.zip')])
    print(done_or_downloading)
    print(f"Done or downloading: {len(done_or_downloading)}")

    for i, link in enumerate(links):
        print(f"Processing link {i+1} of {len(links)} | {link['title']} by {link['artist']}")
        if compose_zip_filename(link['artist'], link['title']) in done_or_downloading:
            print(f"{GREEN}Already downloaded or is downloading, Skipping{END}")
            continue

        page.goto(link['link'])

        download = page.get_by_text('Download', exact=True)
        download.wait_for(state='visible')
        download_url = download.get_attribute('href')
        submit_download_link_to_aria2(str(i+1), download_url)


def submit_download_link_to_aria2(id, link: str):
    r = requests.post('http://localhost:6800/jsonrpc', json={
        'jsonrpc': '2.0',
        'method': 'aria2.addUri',
        'params': ["token:", [link]],
        'id': str(id),
    })
    print(f'{YELLOW}{r.text}{END}')

# extract direct download links
def run_third_stage():
    with open('got.json', 'r') as f:
        got = json.load(f)
    for _, href in got.items():
        print(href)

with sync_playwright() as playwright:
    # pass
    run_preamble(playwright)
    # run_first_stage(playwright)
    # run_second_stage(playwright, os.path.expanduser('~/Music'))
