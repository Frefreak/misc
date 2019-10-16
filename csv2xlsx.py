#!/usr/bin/env python

import pandas as pd
import os
import argparse
import json

parser = argparse.ArgumentParser()
parser.add_argument("file")
parser.add_argument("-o", "--output", metavar="output", help="output filename")
parser.add_argument(
    "-t", "--type", metavar="type info", help="json str of column type info"
)
parser.add_argument(
    "-d",
    "--delimiter",
    metavar="csv_delimiter",
    default=",",
    help="delimiter used in src csv",
)


def main():
    args = parser.parse_args()
    if args.output:
        output = args.output
        if os.path.splitext(output)[1] != ".xlsx":
            output += ".xlsx"
    else:
        output = f"{os.path.splitext(args.file)[0]}.xlsx"

    df = pd.read_csv(args.file, delimiter=args.delimiter)
    if args.type:
        typeinfo = json.loads(args.type)
        for k, ty in typeinfo.items():
            df[k] = df[k].astype(ty)

    df.to_excel(output, index=False)


if __name__ == "__main__":
    main()
