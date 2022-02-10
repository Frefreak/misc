import argparse
import sqlite3
from typing import List, Tuple, Dict
import json
import requests
import readline
from bs4 import BeautifulSoup
from rich.prompt import Prompt
from rich.table import Table
from rich.console import Console

parser = argparse.ArgumentParser()
parser.add_argument('-d', '--db', default='cards.en.cdb')

sess = requests.Session()

console = Console()

def find_text_by_name(cur: sqlite3.Cursor, name: str) -> List[Tuple]:
    cur.execute('select * from texts where name like ?', ('%' + name + '%',))
    return list(cur)

def search_usage(cur: sqlite3.Cursor, name: str):
    result = find_text_by_name(cur, name)
    if len(result) == 0:
        print('\x1b[31;1mno result\x1b[0m')
    elif len(result) == 1:
        do_search(result[0][0])
    else:
        for rec in result:
            print(f'\x1b[31;1m{rec[0]:11}\x1b[0m | \x1b[32;1m{rec[1]}\x1b[0m')
        while True:
            id = Prompt.ask('input id')
            try:
                id = int(id)
                break
            except ValueError:
                print('invalid id')
        do_search(id)


def do_search(id: int):
    resp = sess.get(f'https://www.ourocg.cn/search/{id}/')
    html = BeautifulSoup(resp.text, 'lxml')
    vals = html.find_all('div', class_='val')
    if len(vals) <= 5:
        print('id not found')
        return
    for i in range(3):
        print(f"\x1b[{31+i};1m{vals[i].text.strip()}\x1b[0m", end = '\t')
    print()

    stat = html.find('card-stat-view')
    if stat is None:
        print('not used')
    else:
        stat_data = json.loads(stat.attrs[':stat']) # type: ignore
        console.print(mk_table(stat_data))


def mk_table(stat_data: Dict) -> Table:
    tbl = Table()
    tbl.add_column('更新日期')
    tbl.add_column('周期')
    tbl.add_column('来源')
    tbl.add_column('投入量顺位')
    tbl.add_column('投入1')
    tbl.add_column('投入2')
    tbl.add_column('投入3')
    for obj in stat_data:
        tbl.add_row(str(obj['date']), obj['type'], obj['source'],
                str(obj['index']), str(obj['putone']),
                str(obj['puttwo']), str(obj['putthree']))
    return tbl


def main():
    args = parser.parse_args()
    conn = sqlite3.connect(args.db)
    cursor = conn.cursor()
    while True:
        search_usage(cursor, Prompt.ask('name'))

if __name__ == "__main__":
    main()
    
