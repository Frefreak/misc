#!/usr/bin/env python3

import argparse
import glob
from dataclasses import dataclass
import os
from copy import deepcopy
from shutil import copy
import re
import sys
import string
import time
from collections import namedtuple
from typing import Dict, Optional, List, Tuple

parser = argparse.ArgumentParser()
parser.add_argument('folder',
                    help='root folder of stellaris game (contains common, event...)')
cmd = parser.add_mutually_exclusive_group()
cmd.add_argument('-l', '--list', action='store_true',
                    help='list files to be modified (search "has_dlc" in every file)')
cmd.add_argument('-b', '--backup', action='store_true',
                    help='backup those files to be modified')
cmd.add_argument('-p', '--perform', action='store_true',
                    help='perform the work, will do backup automatically first')
cmd.add_argument('-r', '--restore', action='store_true',
                    help='restore backup files')
cmd.add_argument('-d', '--delete', action='store_true',
                    help='delete backup files')
parser.add_argument('-s', '--suffix',
                    help=('optional backup suffix, required when restore, '
                          'auto (time based) when backup, unless given'))

TAB = '\t'

# https://gist.github.com/eliben/5797351
class Token(object):
    """ A simple Token structure.
        Contains the token type, value and position.
    """
    def __init__(self, type, val, pos):
        self.type = type
        self.val = val
        self.pos = pos

    def __str__(self):
        return '%s(%s) at %s' % (self.type, self.val, self.pos)


class LexerError(Exception):
    """ Lexer error exception.

        pos:
            Position in the input line where the error occurred.
    """
    def __init__(self, pos):
        self.pos = pos


class Lexer(object):
    """ A simple regex-based lexer/tokenizer.

        See below for an example of usage.
    """
    def __init__(self, rules, skip_whitespace=True):
        """ Create a lexer.

            rules:
                A list of rules. Each rule is a `regex, type`
                pair, where `regex` is the regular expression used
                to recognize the token and `type` is the type
                of the token to return when it's recognized.

            skip_whitespace:
                If True, whitespace (\s+) will be skipped and not
                reported by the lexer. Otherwise, you have to
                specify your rules for whitespace, or it will be
                flagged as an error.
        """
        # All the regexes are concatenated into a single one
        # with named groups. Since the group names must be valid
        # Python identifiers, but the token types used by the
        # user are arbitrary strings, we auto-generate the group
        # names and map them to token types.
        #
        idx = 1
        regex_parts = []
        self.group_type = {}

        for regex, type in rules:
            groupname = 'GROUP%s' % idx
            regex_parts.append('(?P<%s>%s)' % (groupname, regex))
            self.group_type[groupname] = type
            idx += 1

        self.regex = re.compile('|'.join(regex_parts))
        self.skip_whitespace = skip_whitespace
        self.re_ws_skip = re.compile('\S')

    def input(self, buf):
        """ Initialize the lexer with a buffer as input.
        """
        self.buf = buf
        self.pos = 0

    def token(self):
        """ Return the next token (a Token object) found in the
            input buffer. None is returned if the end of the
            buffer was reached.
            In case of a lexing error (the current chunk of the
            buffer matches no rule), a LexerError is raised with
            the position of the error.
        """
        if self.pos >= len(self.buf):
            return None
        else:
            if self.skip_whitespace:
                m = self.re_ws_skip.search(self.buf, self.pos)

                if m:
                    self.pos = m.start()
                else:
                    return None

            m = self.regex.match(self.buf, self.pos)
            if m:
                groupname = m.lastgroup
                tok_type = self.group_type[groupname]
                tok = Token(tok_type, m.group(groupname), self.pos)
                self.pos = m.end()
                return tok

            # if we're here, no rule matched
            raise LexerError(self.pos)

    def tokens(self):
        """ Returns an iterator to the tokens found in the buffer.
        """
        while 1:
            tok = self.token()
            if tok is None: break
            yield tok


rules = [
    ('#.*',         'COMMENT'),
    ('{',           'OPEN_BRACKET'),
    ('}',           'CLOSE_BRACKET'),
    ('<=|>=|=|<|>', 'RELATION'),
    ('".*?"',       'QUOTE_STR'),
    ('-?\d+\.\d+',  'NUM'),
    ('[^\s{}=><]+', 'STR'),
]


class Element:
    def show(self, indent: int, newline: bool) -> str:
        raise Exception("unimplemented")

@dataclass
class Str(Element):
    val: str
    def show(self, indent: int, newline: bool) -> str:
        if newline:
            return f'\n{indent * TAB}{self.val}'
        else:
            return self.val

@dataclass
class Struct(Element):
    name: str
    val: Element

    def show(self, indent: int, newline: bool) -> str:
        content = self.val.show(indent + 1, False)
        if newline:
            return f'\n{indent * TAB}{self.name} {content}'
        else:
            return f'{self.name} {content}'


@dataclass
class KV(Element):
    key: str
    rel: str
    val: Element

    def show(self, indent: int, newline: bool) -> str:
        content = self.val.show(indent + 1, False)
        if newline:
            return f'\n{indent * TAB}{self.key} {self.rel} {content}'
        else:
            return f'{self.key} {self.rel} {content}'

@dataclass
class LI(Element):
    val: list

    def show(self, indent: int, newline: bool) -> str:
        s = f'\n{indent * TAB}' if newline else ''
        s += '{'
        for e in self.val:
            s += e.show(indent, True)
        if newline:
            s += f'\n{(indent) * TAB}}}'
        else:
            s += f'\n{(indent - 1) * TAB}}}'
        return s

    def set(self, i, e):
        self.val[i] = e

def get_token(s: str) -> List[str]:
    lx = Lexer(rules, skip_whitespace=True)
    lx.input(s)
    return [t.val for t in lx.tokens() if t.type != 'COMMENT']

def parse_txt(s: str) -> List[Element]:
    if s[0] == '\ufeff':    # remove BOM header if exist
        s = s[1:]
    wds = get_token(s)
    result: List[Element] = []
    while wds:
        result.append(parse_element(wds))
    return result

def parse_struct(wds: List[str]) -> Struct:
    name = wds[0]
    wds.pop(0)
    val = parse_element(wds)
    return Struct(name, val)


def parse_kv(wds: List[str]) -> KV:
    key = wds[0]
    rel = wds[1]
    wds[:] = wds[2:]
    val = parse_element(wds)
    return KV(key, rel, val)


def parse_li(wds: List[str]) -> LI:
    wds.pop(0)
    val = []
    while wds[0] != '}':
        val.append(parse_element(wds))
    wds.pop(0)
    return LI(val)

def parse_str(wds: List[str]):
    s = wds.pop(0)
    return Str(s)

def parse_element(wds: List[str]) -> Element:
    if len(wds) == 1:
        return parse_str(wds)
    if wds[0] == '{':
        return parse_li(wds)
    elif wds[1] == '{':
        return parse_struct(wds)
    elif wds[1] in ['=', '<', '>', '<=', '>=']:
        return parse_kv(wds)
    else:
        return parse_str(wds)


def format_txt(result: List) -> str:
    s = ''
    for r in result:
        s += r.show(0, True)
    return s


def glob_pat(pattern: str, folder: str) -> List[str]:
    common = os.path.join(folder, 'common', '**', f'*{pattern}')
    events = os.path.join(folder, 'events', '**', f'*{pattern}')
    filelist = glob.glob(common, recursive=True)
    filelist.extend(glob.glob(events, recursive=True))
    return filelist


def has_dlc_in_ele(e: Element) -> bool:
    if isinstance(e, Str):
        return False
    elif isinstance(e, Struct):
        return has_dlc_in_ele(e.val)
    elif isinstance(e, KV):
        if e.key == 'host_has_dlc' or e.key == 'local_has_dlc':
            return True
        else:
            return has_dlc_in_ele(e.val)
    elif isinstance(e, LI):
        return any([has_dlc_in_ele(ee) for ee in e.val])
    else:
        raise Exception("unknown element type")


def has_dlc_in(cont: List[Element]) -> bool:
    for e in cont:
        if has_dlc_in_ele(e):
            return True
    return False


def get_has_dlc_file(args) -> List[str]:
    batch = 10
    files = glob_pat('.txt', args.folder)
    print(f'parsing {len(files)} files, every "." is {batch} files')
    target = []
    failed = []
    for i, txt in enumerate(files):
        with open(txt) as f:
            try:
                content = parse_txt(f.read())
            except Exception as e:
                print(f'error parsing {txt}: {repr(e)}')
                failed.append(txt)
            if has_dlc_in(content):
                target.append(txt)
        if i % batch == batch - 1:
            sys.stdout.write('.')
            sys.stdout.flush()

    print()
    print('failed:')
    for fail in failed:
        print(fail)
    return target


def mk_real_suffix(suffix):
    return '.' + suffix + '.bak'

def mk_bk_fn(f, suffix):
    return f + mk_real_suffix(suffix)

def mk_rs_fn(f, suffix):
    if f.endswith((tail := mk_real_suffix(suffix))):
        return f.rstrip(tail)

def real_backup(files, args):
    suffix = args.suffix
    print(f'suffix: {suffix}')
    for f in files:
        new_name = mk_bk_fn(f, suffix)
        copy(f, new_name)


def modify_content(e: Element):
    if isinstance(e, Str):
        return False
    elif isinstance(e, Struct) or isinstance(e, KV):
        modify_content(e.val)
    elif isinstance(e, LI):
        for i, ee in enumerate(e.val):
            if isinstance(ee, KV) and ee.key in ['local_has_dlc', 'host_has_dlc']:
                e.set(i, KV('NOT', '=', LI([ee])))
            elif isinstance(ee, KV) and ee.key == 'NOT' and ee.val.val[0].key in \
                    ['local_has_dlc', 'host_has_dlc']:
                e.set(i, ee.val.val[0])
            else:
                modify_content(ee)
    else:
        raise Exception("unknown element type")


def real_perform(txt):
    with open(txt) as f:
        try:
            content = parse_txt(f.read())
        except Exception as e:
            print(f'error parsing {txt}: {repr(e)}, this should not happen')
            exit(1)
    for cont in content:
        modify_content(cont)
    s = format_txt(content)
    with open(txt, 'w') as f:
        f.write(s)

def do_list(args):
    files = get_has_dlc_file(args)
    for f in files:
        print(f)
    print('total: {len(files)}')


def do_delete(args):
    files = glob_pat(mk_real_suffix(args.suffix), args.folder)
    for f in files:
        print(f)
    if not files:
        print('no files meets pattern')
        exit(0)
    input(f'total: {len(files)} files, continue?')
    input('for real???')
    for f in files:
        os.unlink(f)
    print('done')


def do_restore(args):
    files = glob_pat(mk_real_suffix(args.suffix), args.folder)
    for f in files:
        print(f)
    if not files:
        print('no files meets pattern')
        exit(0)
    input(f'total: {len(files)} files, continue?')
    for f in files:
        orig = mk_rs_fn(f, args.suffix)
        copy(f, orig)


def do_backup(args):
    files = get_has_dlc_file(args)
    print('files to backup:')
    for f in files:
        print(f)
    input(f'total: {len(files)}, Enter to continue')
    real_backup(files, args)
    print('done. please check manually')


def do_perform(args):
    files = get_has_dlc_file(args)
    for f in files:
        print(f)
    input(f'total: {len(files)}, will do backup, Enter to continue')
    real_backup(files, args)
    input('modification will startup, reverse has dlc logic, Enter to continue')
    for f in files:
        real_perform(f)
    print('done')


def main():
    args = parser.parse_args(sys.argv[1:])
    if (args.restore or args.delete) and not args.suffix:
        print('need suffix when restore or delete, provide with "-s"', file=sys.stderr)
        exit(1)
    if not args.suffix and (args.perform or args.backup):
        args.suffix = time.strftime('%Y%m%d_%H%M%S')
    if args.list:
        do_list(args)
    elif args.restore:
        do_restore(args)
    elif args.backup:
        do_backup(args)
    elif args.delete:
        do_delete(args)
    elif args.perform:
        do_perform(args)
    else:
        print("don't know what to do, help with -h", file=sys.stderr)
        exit(0)

if __name__ == "__main__":
    main()
    #  real_perform('test.txt')
