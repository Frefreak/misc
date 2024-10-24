# steal from web
#!/bin/python
# coding=utf-8

import urllib.request, urllib.parse, urllib.error
import urllib.request, urllib.error, urllib.parse
import json
import codecs
import time
import sys
import os
import argparse



def game_list(lastCode):
    values = {
    "type": 4,
    "lastCode": lastCode,
    "uid": uid, 
    "RelationTag": 0}
    data = urllib.parse.urlencode(values).encode("utf-8")
    url = "http://happyapp.huanle.qq.com/cgi-bin/CommonMobileCGI/TXWQFetchChessList"
    request = urllib.request.Request(url, data)
    response = urllib.request.urlopen(request)
    chesslist = json.loads(response.read().decode("utf-8"))['chesslist']

    chessid = []
    fn = []
      

    for d in chesslist:
        starttime = d['starttime'].split(' ')[0].replace('-', '.')
        if end is not None and end < starttime:
            continue
        if start is not None and start > starttime:
            break
        chessid.append(d['chessid'])    
        id = d['chessid'][10:]
        blackenname = d['blackenname']
        whiteenname = d['whiteenname']
        fn.append(USERNAME+'/'+starttime + ' ' + blackenname + ' VS ' + whiteenname + ' (' + id + ')' + '.SGF')
    return chessid, fn

def download_sgf(cid, fn):
    values = {
    "chessid": cid
    }
    data = urllib.parse.urlencode(values).encode("utf-8")
    url = "http://happyapp.huanle.qq.com/cgi-bin/CommonMobileCGI/TXWQFetchChess"
    sgf = ""
    for i in range(10):
        try:
            request = urllib.request.Request(url, data)
            response = urllib.request.urlopen(request)
            sgf = json.loads(response.read().decode("utf-8"))['chess']
            break
        except Exception as e:
            if i == 9:
                print(e)
                sys.exit(1)

    f = codecs.open(fn, 'w', 'utf-8')
    f.write(sgf)
    f.close()

def download_all_user_sgf():
    chessid, fn = game_list("")
    idx = 0
    for i in range(len(chessid)):
        download_sgf(chessid[i], fn[i])
        print(i + 1, fn[i])
        time.sleep(0.5)

    while True:
        lastCode = chessid[-1]
        idx += len(chessid)
        chessid, fn = game_list(lastCode)

        if len(chessid) == 0:
            break

        for i in range(len(chessid)):
            download_sgf(chessid[i], fn[i])
            print(i + idx + 1, fn[i])
            time.sleep(0.5)

parser = argparse.ArgumentParser()
parser.add_argument('--username', type=str, help = 'e.g. --username 绝艺')
parser.add_argument('--start', type=str, help = 'e.g. --start 2019.01.01')
parser.add_argument('--end', type=str, help = 'e.g. --end 2020.01.01')
args = parser.parse_args()
USERNAME = args.username
start = args.start
end = args.end

try:
    USERNAME_URL = urllib.parse.quote(USERNAME)
except:
    print('Please input username')
    print('e.g.：--username 柯洁')
    exit()
    
url = f"http://h5.foxwq.com/getFechInfo/wxnseed/txwq_fetch_personal_info?srcuid=0&dstuid=0&dstuin=0&username={USERNAME_URL}&accounttype=0&clienttype=0"
request = urllib.request.Request(url)
response = urllib.request.urlopen(request)
uid = str(json.loads(response.read())['uid'])
if not os.path.isdir(USERNAME):
    os.mkdir(USERNAME)

download_all_user_sgf()
