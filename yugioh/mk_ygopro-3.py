#!/usr/bin/env python3

import os
import sys
import sqlite3

dest = os.getenv('DEST', None)
if dest is None:
    print('DEST is empty')
    sys.exit(1)
game_dir = os.path.join(dest, 'game')
pics_dir = os.path.join(dest, 'pics')
os.makedirs(pics_dir, exist_ok=True)

cdb = os.path.join(game_dir, 'cards.cdb')
db = sqlite3.connect(cdb)
cur = db.cursor()
ids = [x[0] for x in cur.execute('select id from datas')]
cards_url = os.path.join(dest, 'cards_url.txt')
with open(cards_url, 'w') as f:
    for id in ids:
        # use http to avoid cert issue for now
        f.write(f'http://cdn.233.momobako.com/ygopro/pics/{id}.jpg\n')
print('now invoke aria2c with:')
print(f'aria2c -c -i {cards_url} --dir {pics_dir} -j4')
