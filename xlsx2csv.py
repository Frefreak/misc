#!/usr/bin/env python

import os
import argparse
import pandas as pd

parser = argparse.ArgumentParser()
parser.add_argument("file")
parser.add_argument("-o", "--output", metavar="output", help="output filename")
parser.add_argument(
    "-r",
    "--remove",
    metavar="remove_columns",
    help="remove columns(comma seperated)",
)


def main():
    args = parser.parse_args()
    if args.output:
        output = args.output
        if os.path.splitext(output)[1] != ".csv":
            output += ".csv"
    else:
        output = f"{os.path.splitext(args.file)[0]}.csv"

    df = pd.read_excel(args.file)
    if args.remove:
        columns = list(map(str.strip, args.remove.split(",")))
        df.drop(columns=columns, inplace=True, axis=1)
    df.to_csv(output, index=False)


if __name__ == "__main__":
    main()
