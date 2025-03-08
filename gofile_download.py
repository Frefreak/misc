#!/usr/bin/env python3

import argparse
import requests
import sys
import re
import json
import random

# Parse command line arguments
parser = argparse.ArgumentParser(description='GoFile Download Script')
parser.add_argument('url', help='The GoFile URL to download from')
args = parser.parse_args()

# Main function
def main():
    try:
        # Extract the last path segment from the URL
        url_segment = args.url.split('/')[-1]

        # Step 1: Query for token
        token_response = requests.post('https://api.gofile.io/accounts')
        token_response.raise_for_status()
        token = token_response.json()['data']['token']

        # Step 2: Get wt from global.js
        global_js_response = requests.get('https://gofile.io/dist/js/global.js')
        global_js_response.raise_for_status()
        wt_match = re.search(r'appdata\.wt\s*=\s*"([^"]+)"', global_js_response.text)
        if not wt_match:
            raise ValueError('Could not find wt in global.js')
        wt = wt_match.group(1)

        # Step 3: Query contents API
        contents_url = f'https://api.gofile.io/contents/{url_segment}?wt={wt}&contentFilter=&page=1&pageSize=1000&sortField=name&sortDirection=1'
        contents_headers = {'Authorization': f'Bearer {token}'}
        contents_response = requests.get(contents_url, headers=contents_headers)
        contents_response.raise_for_status()

        # Process the response
        contents = contents_response.json()['data']['children']
        file_info = []
        download_urls = []

        for item_id, item in contents.items():
            if item['type'] == 'file':
                file_info.append({
                    'id': item_id,
                    'name': item['name'],
                    'servers': item['servers']
                })
                print(item['name'])

                # Choose a random server
                chosen_server = random.choice(item['servers'])

                # Compose download URL with cookie
                download_url = f'https://{chosen_server}.gofile.io/download/web/{item_id}/{item["name"].replace(" ", "%20")}'
                download_url_with_cookie = f'{download_url}'
                download_urls.append(download_url_with_cookie)

        # Save file info for later use
        with open('/home/adv_zxy/temp/file_info.json', 'w') as f:
            json.dump(file_info, f)

        # Save download URLs to a file with comments
        with open('/home/adv_zxy/temp/download_list.txt', 'w') as f:
            for url in download_urls:
                filename = url.split('/')[-1].split(' ')[0].replace('%20', ' ')
                f.write(f'# {filename}\n{url}\n')
        print('aria2c --input-file=download_list.txt --header="Cookie: accountToken={token}"')

    except requests.RequestException as e:
        print(f'Error fetching URL: {e}', file=sys.stderr)
        sys.exit(1)
    except Exception as e:
        print(f'An error occurred: {e}', file=sys.stderr)
        sys.exit(1)

if __name__ == '__main__':
    main()
