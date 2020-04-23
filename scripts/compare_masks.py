#!/usr/bin/env python3

import argparse
import csv

from functools import reduce
from itertools import takewhile
from pathlib import Path
from statistics import mean, median

def cr_to_tuple(string):
    col = list(takewhile(lambda c: c.isalpha(), string))
    row = int(string[len(col):]) - 1
    col = reduce(lambda a,c: 26*a + ord(c) - 64, col, 0) - 1
    return (col,row)

def cr_range_to_tuple(string):
    c1r1 , c2r2 = string.split(':')
    return (cr_to_tuple(c1r1),cr_to_tuple(c2r2))

def dir_path(string):
    path = Path(string)
    if path.is_dir(): return path
    raise NotADirectoryError(string)

def contains(col, row, tl, br):
    return (tl[1] <= col <= br[1]) and (tl[0] <= row <= br[0])

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument('-g', '--ground-truth-dir', type=dir_path, required=True)
    parser.add_argument('-p', '--prediction-dir', type=dir_path, required=True)
    parser.add_argument('-o', '--pn-output-dir', type=dir_path, required=True)
    parser.add_argument('-c', '--table-range-column', type=int, required=False)
    parser.add_argument('-t', '--tables-data-csv', type=argparse.FileType('r'), required=False)
    args = parser.parse_args()

    if (args.tables_data_csv is None) != (args.table_range_column is None):
        print('-c and -t must be used together.')
        exit(1)

    ap , ar , af = [], [], []

    print(f'{"FILENAME":80}, TRUE_POS,FALSE_POS,FALSE_NEG,PRECISION,   RECALL, F1-SCORE')
    print(f"{'='*80},{'='*9},{'='*9},{'='*9},{'='*9},{'='*9},{'='*9}")

    todo = {}
    if args.tables_data_csv is None:
        for gt_path in sorted(args.ground_truth_dir.glob('*.csv')):
            todo[gt_path] = None
    else:
        for i, row in enumerate(csv.reader(args.tables_data_csv)):
            gt_path = args.ground_truth_dir.joinpath(row[0].strip())
            if not gt_path.is_file():
                continue

            table = row[args.table_range_column].strip()
            if not table or table == '<null>':
                continue

            table = cr_range_to_tuple(table)
            if gt_path in todo:
                todo[gt_path].add(table)
            else:
                todo[gt_path] = {table}

    for gt_path, tables in todo.items():
        pred_path = args.prediction_dir.joinpath(gt_path.name)
        if not pred_path.is_file():
            print(f'{gt_path.name:80},    -    ,    -    ,    -    ,    -    ,    -    ,    -    ')
            continue

        tp , fp , fn = 0.0 , 0.0 , 0.0

        with open(gt_path, 'r') as gt_file:
            gt_csv = csv.reader(gt_file)

            with open(pred_path, 'r') as pred_file:
                pred_csv = csv.reader(pred_file)

                with open(args.pn_output_dir.joinpath(gt_path.name), 'w') as pn_mask_file:
                    pn_mask_csv = csv.writer(pn_mask_file)

                    row = 0
                    while True:
                        try:
                            pn_row = []
                            gt_row = next(gt_csv)
                            pred_row = next(pred_csv)
                            for col,(gt_cell,pred_cell) in enumerate(zip(gt_row,pred_row)):
                                if tables is not None and not any(contains(col, row, *t) for t in tables):
                                    pn_row.append('..')
                                    continue

                                if gt_cell == '':
                                    if pred_cell != '':
                                        fp += 1
                                        pn_row.append('FP')
                                    else:
                                        pn_row.append('--')
                                else:
                                    if pred_cell == '':
                                        fn += 1
                                        pn_row.append('FN')
                                    else:
                                        tp += 1
                                        pn_row.append('TP')
                            pn_mask_csv.writerow(pn_row)
                        except StopIteration:
                            break

        if tp > 0 and tp + fp > 0.0 and tp + fn > 0.0:
            precision = tp / (tp + fp)
            recall = tp / (tp + fn)
            fscore = (2.0 * precision * recall) / (precision + recall)

            ap.append(precision)
            ar.append(recall)
            af.append(fscore)

            print(f'{gt_path.name:80},{tp:9.3f},{fp:9.3f},{fn:9.3f},  {precision:5.3f}  ,  {recall:5.3f}  ,  {fscore:5.3f}')
        else:
            print(f'{gt_path.name:80},{tp:9.3f},{fp:9.3f},{fn:9.3f},    X    ,    X    ,    X    ')

    if len(ap) > 0:
        print(f"{'='*80},{'='*9},{'='*9},{'='*9},{'='*9},{'='*9},{'='*9}")
        print(f'{"<MIN>":80},    -    ,    -    ,    -    ,  {min(ap):5.3f}  ,  {min(ar):5.3f}  ,  {min(af):5.3f}')
        print(f'{"<AVG>":80},    -    ,    -    ,    -    ,  {mean(ap):5.3f}  ,  {mean(ar):5.3f}  ,  {mean(af):5.3f}')
        print(f'{"<MED>":80},    -    ,    -    ,    -    ,  {median(ap):5.3f}  ,  {median(ar):5.3f}  ,  {median(af):5.3f}')
        print(f'{"<MAX>":80},    -    ,    -    ,    -    ,  {max(ap):5.3f}  ,  {max(ar):5.3f}  ,  {max(af):5.3f}')