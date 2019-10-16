#!/usr/bin/env python

import re
import os
import yaml
import requests
from bs4 import BeautifulSoup

import pan

def simple_get(url):
    r = requests.get(url)
    r.encoding = 'utf-8'
    return BeautifulSoup(r.text, 'lxml')

def mjt_search(name):
    mjt_search_url = 'http://www.mjt100.com/search.php'
    r = requests.post(mjt_search_url, data={'searchword': name})
    html = BeautifulSoup(r.text, 'lxml')
    # default search newest first
    try:
        href = html.select('div.index-area.clearfix')[0].li.a['href']
        return 'http://www.mjt100.com' + href
    except AttributeError:
        return None

# baidu pan link
def get_sharelink(url):
    html = simple_get(url)
    purl = 'http://www.mjt100.com' + \
                html.select('div.plist.yun')[0].select('a')[-1]['href']
    html = simple_get(purl)
    js = html.select('div.player script')[0].text
    urlstrs = [x for x in js.split('$') if re.search('pan.baidu.com', x)]
    url_dict = {}
    for us in urlstrs:
        kv = us.split('|')
        url_dict[kv[0]] = kv[1].split(':')[1]
    return url_dict

# just store all tv-series name line by line
def read_config(fn):
    with open(fn, 'r') as f:
        return list(map(str.strip, f))

def load_storage(fn):
    with open(fn, 'r') as f:
        y = yaml.load(f.read())
        if not y:
            y = {}
        return y

def save_storage(obj, fn):
    with open(fn, 'w') as f:
        yaml.dump(obj, f)

if __name__ == "__main__":
    cur_path = os.path.abspath(os.path.join(__file__, os.path.pardir))
    os.chdir(cur_path)
    storage_file = './storage.yaml'
    root_dir = '/tv_series'
    storage = load_storage(storage_file)
    try:
        for tv in read_config('./tv.conf'):
            print(f'querying \x1b[3;33m{tv}\x1b[0m...')
            url = mjt_search(tv)
            if not url:
                print('\x1b[3;33m search failed\x1b[0m.')
                continue
            links = get_sharelink(url)
            tv_stor = storage.get(tv)
            if not tv_stor:
                tv_stor = {}
            for i, (k, v) in enumerate(links.items()):
                if k not in tv_stor.keys():
                    print(f'{k}:{v} (\x1b[33;1m{i+1}/{len(links)}\x1b[0m)',
                          end='')
                    t_folder = root_dir + '/' + re.sub(' ', '_', tv.lower())
                    if pan.store_share_link(k, v, t_folder, verbose=False):
                        print('\x1b[32;1m success\x1b[0m')
                        tv_stor.update({k:v})
                        storage[tv] = tv_stor
                    else:
                        print('\x1b[31;1m fail\x1b[0m')
                else:
                    print(f'{k}:{v} ({i+1}/{len(links)}), skipping...')
    except KeyboardInterrupt:
        pass
    finally:
        print('saving to storage...')
        save_storage(storage, storage_file)
        print('done.')
