#!/usr/bin/env python

import os
import sys
import argparse

parser = argparse.ArgumentParser()
parser.add_argument('direction', type=str, choices=['next', 'prev'])


def _inspect(offset):
    all_siblings = os.listdir('..')
    cur = os.path.abspath(os.curdir)
    dir_name = cur.split('/')[-1]
    cur_index = dir_name.split('_')[0]
    width = len(cur_index)
    target_index = int(cur_index) + offset
    target_prefix = f'{target_index:0{width}d}'

    for subf in all_siblings:
        if subf.split('_')[0] == target_prefix:
            sys.stderr.write(f'cd {subf}\n')
            return f'../{subf}'
    sys.stderr.write("folder switch failed\n")
    return '.'

def inspect_next():
    print(_inspect(1))

def inspect_prev():
    print(_inspect(-1))

def main():
    args = parser.parse_args(sys.argv[1:])
    if args.direction == 'next':
        inspect_next()
    else:
        inspect_prev()

if __name__ == "__main__":
    main()
