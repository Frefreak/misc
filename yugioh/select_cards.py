#!/usr/bin/env python3

import sys
import os
from shlex import quote
import argparse
import sqlite3
import subprocess

parser = argparse.ArgumentParser()
parser.add_argument("-d", "--delim", default=",")
parser.add_argument("-f", "--filter", default="")

cdb = os.getenv("CARDS_CDB", None)
if cdb is None:
    print("CARDS_CDB")
    sys.exit(1)


def main():
    args = parser.parse_args()
    cur = sqlite3.connect(cdb).cursor()
    id_maps = {x[0]: x[1] for x in cur.execute("select id, name from texts")}
    inp = [str(k) + ":" + v for (k, v) in id_maps.items()]
    input_str = "\n".join(inp)
    opts = ""
    if args.filter:
        opts += f" -f {quote(args.filter)} "
    p = subprocess.Popen(
        f"fzf -d':' {opts} -m --with-nth 2..",
        shell=True,
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
    )
    r = p.communicate(input_str.encode())
    selected = [x.split(":")[0] for x in r[0].decode().splitlines()]
    print(args.delim.join(selected))


if __name__ == "__main__":
    main()
