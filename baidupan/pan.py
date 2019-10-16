from functools import reduce
from operator import add
import re
from json import loads, dumps
import requests

baidupan_headers = """
Origin: https://pan.baidu.com
X-Requested-With: XMLHttpRequest
User-Agent: Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.81 Safari/537.36
Accept: application/json, text/javascript, */*; q=0.01
Accept-Encoding: gzip, deflate, br
Referer: https://pan.baidu.com/disk/home
"""
baidupan_querystr = {'channel': 'chunlei', 'clienttype': '0',
                     'app_id': '250528'}

real_cache = """
PSTM
BAIDUID
BIDUPSID
BDUSS
BDORZ
BDRCVFR[feWj1Vr5u3D]
PSINO
H_PS_PSSID
cflag
pcsett
STOKEN
""".splitlines()
real_cache = [x.strip() for x in real_cache if x != '']

def mkHeader(hdrs):
    header = {}
    for kv in map(lambda x: x.split(':'), hdrs.splitlines()):
        if kv[0]:
            header[kv[0].strip()] = reduce(add, kv[1:]).strip()
    return header

def getWithHeader(url, hdrs):
    hdr = mkHeader(hdrs)
    r = requests.get(url, headers=hdr)
    return r.text

def parseCookie(cookie):
    cookies = cookie.split(';')
    coo = {}
    for c in cookies:
        tup = c.split('=')
        coo[tup[0].strip()] = tup[1].strip()
    return coo

def get_baidupan_cookie():
    cookie_file = '/home/adv_zxy/.cache/ZXY/baidu_netdisk'
    with open(cookie_file, 'r') as f:
        cookie = parseCookie(f.read())
        return {cook:cookie[cook] for cook in cookie if cook in real_cache}

def share_init(url):
    sess = requests.session()
    #  sess.verify = False
    #  sess.proxies.update({'https': 'http://127.0.0.1:8080', 'http': 'http://127.0.0.1:8080'})
    sess.headers = mkHeader(baidupan_headers)
    sess.cookies.update(get_baidupan_cookie())
    r = sess.get(url)
    r.encoding = 'utf-8'
    try:
        params = r.url.split('?')[1].split('&')
    except IndexError:
        return None
    params = {x.split('=')[0]:x.split('=')[1] for x in params}
    stoken = re.search(r'"bdstoken":"([0-9a-f]+)"', r.text).groups()[0]
    return sess, params, stoken

def verify_vcode(sess, code, params, stoken):
    verify_url = 'https://pan.baidu.com/share/verify'
    baidupan_querystr.update({'bdstoken': stoken})
    baidupan_querystr.update(params)
    r = sess.post(verify_url, params=baidupan_querystr,
                  data={'pwd': code, 'vcode': '', 'vcode_str': ''})
    r.encoding = 'utf-8'
    return sess

def share_link(sess, params):
    link_url = 'https://pan.baidu.com/share/link'
    r = sess.get(link_url, params=params,
                 headers=mkHeader(baidupan_headers))
    r.encoding = 'utf-8'
    search = re.search(r'yunData.setData\((.*)\);', r.text)
    json = loads(search.groups()[0])
    try:
        filepaths = [x['path'] for x in json['file_list']['list']]
        return sess, filepaths
    except KeyError:
        return sess, None

def list_dir(sess, bdstoken, dirname):
    apiurl = 'https://pan.baidu.com/api/list'
    baidupan_querystr.update({'web': '1', 'bdstoken': bdstoken,
                              'dir': dirname})
    r = sess.get(apiurl, params=baidupan_querystr)
    json = loads(r.text)
    if json['errno'] == -9:
        return None
    dirs = [x['path'] for x in json['list'] if x['isdir'] == 1]
    files = [x['path'] for x in json['list'] if x['isdir'] == 0]
    return {'file': files, 'dir': dirs}

def create_dir(sess, bdstoken, path):
    apiurl = 'https://pan.baidu.com/api/create'
    baidupan_querystr.update({'a': 'commit', 'bdstoken': bdstoken})
    r = sess.post(apiurl, params=baidupan_querystr,
            data={'path': path, 'isdir': 1, 'method': 'post'})
    return loads(r.text)

def share_transfer(sess, params, stoken, src_paths, dest_path):
    if dest_path[0] != '/':
        dest_path = '/' + dest_path
    if not list_dir(sess, stoken, dest_path):
        create_dir(sess, stoken, dest_path)
    transfer_url = 'https://pan.baidu.com/share/transfer'
    baidupan_querystr.update({'web': '1', 'bdstoken': stoken,
                 'shareid': params['shareid'], 'from': params['uk']})
    r = sess.post(transfer_url, params=baidupan_querystr,
                  data={'filelist': dumps(src_paths),
                        'path': dest_path})
    return r


# use this function
def store_share_link(url, share_pwd, dest_path, verbose=True):
    status = share_init(url)
    if not status:
        if verbose:
            print('link not work')
        return False
    sess, par, s = status
    sess = verify_vcode(sess, share_pwd, par, s)
    sess, fps = share_link(sess, par)
    if not fps: # query error
        print('TODO')
        return False
    r = share_transfer(sess, par, s, fps, dest_path)
    errno = loads(r.text)['errno']
    if errno == 0 or errno == 12: # 12: already present
        if verbose:
            print('transfer successfully')
        return True
    else:
        if verbose:
            print(f'failed, errno: {errno}')
        return False
