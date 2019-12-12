#!/usr/bin/env python

import argparse
import os
import requests

parser = argparse.ArgumentParser()
parser.add_argument('file')
parser.add_argument('-p', '--prefix')
parser.add_argument('-o', '--output')


def parse(txt, prefix):
    lns = txt.splitlines()
    resources = []
    total_time = 0
    i = 0
    while i < len(lns):
        if lns[i].startswith('#EXTINF:'):
            resources.append(os.path.join(prefix, lns[i+1]))
            i += 1
        i += 1
    return resources

def main():
    args = parser.parse_args()
    if args.file.startswith('http'):
        args.prefix = os.path.dirname(args.file)
    else:
        if args.prefix is None:
            print("prefix is required when given file")
            os._exit(1)

    if args.file.startswith('http'):
        r = requests.get(args.file)
        content = r.text
    else:
        with open(args.file) as f:
            content = f.read()
    resources = parse(content, args.prefix)
    if args.output:
        with open(args.output, 'w') as f:
            f.write('\n'.join(resources))
    else:
        print('\n'.join(resources))


if __name__ == "__main__":
    main()
