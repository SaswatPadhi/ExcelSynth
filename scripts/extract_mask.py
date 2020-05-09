#!/usr/bin/env python3

import argparse
import csv

from pathlib import Path

def dir_path(string):
    path = Path(string)
    if path.is_dir(): return path
    raise NotADirectoryError(string)

if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('-i', '--input-dir', type=dir_path, required=True)
    parser.add_argument('-o', '--output-dir', type=dir_path, required=True)
    args = parser.parse_args()

    for src in args.input_dir.glob('*.csv'):
        dst = args.output_dir.joinpath(src.name)
        with open(src, 'r') as src_file:
            src_csv = csv.reader(src_file)
            with open(dst, 'w') as dst_file:
                dst_csv = csv.writer(dst_file)
                dst_csv.writerows(((field if field.startswith('=') else "") for field in row)
                                  for row in src_csv)