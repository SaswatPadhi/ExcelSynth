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
    return (tl[0] <= col <= br[0]) and (tl[1] <= row <= br[1])

def print_PRF(label, tables, tp, fp, fn):
    if tp + fp > 0:
        precision = tp / (tp + fp)
        precision_str = f'{precision:5.3f}'
    else:
        precision = -1
        precision_str = '  X  '
    if tp + fn > 0:
        recall = tp / (tp + fn)
        recall_str = f'{recall:5.3f}'
    else:
        recall = -1
        recall_str = '  X  '

    if precision > 0 and recall > 0:
        fscore = (2 * precision * recall) / (precision + recall)
        print(f'{label:80},  {tables:3}  ,  {tp:5d}  ,  {fp:5d}  ,  {fn:5d}  ,  {precision_str}  ,  {recall_str}  ,  {fscore:5.3f}  ')
    else:
        print(f'{label:80},  {tables:3}  ,  {tp:5d}  ,  {fp:5d}  ,  {fn:5d}  ,  {precision_str}  ,  {recall_str}  ,    X    ')

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument('-g', '--ground-truth-dir', type=dir_path, required=True)
    parser.add_argument('-p', '--prediction-dir', type=dir_path, required=True)
    parser.add_argument('-o', '--output-dir', type=dir_path, required=True)
    parser.add_argument('-r', '--ground-truth-table-column', type=int, required=True)
    parser.add_argument('-t', '--tables-data-csv', type=argparse.FileType('r'), required=True)
    parser.add_argument('-c', '--table-range-column', type=int, required=False)
    args = parser.parse_args()

    TP, FP, FN = 0, 0, 0
    S_TP, S_FP, S_FN = 0, 0, 0
    M_TP, M_FP, M_FN = 0, 0, 0
    TABLES, S_TABLES, M_TABLES = 0, 0, 0

    print(f'{"FILENAME":80}, TABLES, TRUE_POS,FALSE_POS,FALSE_NEG,PRECISION,   RECALL, F1-SCORE')
    print(f"{'='*80},{'='*7},{'='*9},{'='*9},{'='*9},{'='*9},{'='*9},{'='*9}")

    gt_tables_count, todo = {}, {}
    for row in csv.reader(args.tables_data_csv):
        gt_path = args.ground_truth_dir.joinpath(row[0].strip())
        if not gt_path.is_file():
            continue

        gt_table = row[args.ground_truth_table_column].strip()
        if gt_table and gt_table != '<null>':
            if gt_path in gt_tables_count:
                gt_tables_count[gt_path] += 1
            else:
                gt_tables_count[gt_path] = 1

        if args.table_range_column is not None:
            table = row[args.table_range_column].strip()
            if not table or table == '<null>':
                continue

            table = cr_range_to_tuple(table)
            if gt_path in todo:
                todo[gt_path].add(table)
            else:
                todo[gt_path] = {table}
        else:
            todo[gt_path] = None

    for gt_path, tables in todo.items():
        pred_path = args.prediction_dir.joinpath(gt_path.name)
        if not pred_path.is_file():
            print(f'{gt_path.name:80},   -   ,    -    ,    -    ,    -    ,    -    ,    -    ,    -    ')
            continue

        tp, fp, fn = 0, 0, 0

        with open(gt_path, 'r') as gt_file:
            gt_csv = csv.reader(gt_file)

            with open(pred_path, 'r') as pred_file:
                pred_csv = csv.reader(pred_file)

                with open(args.output_dir.joinpath(gt_path.name), 'w') as output_mask_file:
                    output_mask_csv = csv.writer(output_mask_file)

                    row = -1
                    while True:
                        try:
                            row += 1
                            output_row = []
                            gt_row = next(gt_csv)
                            pred_row = next(pred_csv)
                            for col,(gt_cell,pred_cell) in enumerate(zip(gt_row,pred_row)):
                                if tables is not None and not any(contains(col, row, *t) for t in tables):
                                    output_row.append('XX')
                                    continue

                                if gt_cell == '':
                                    if pred_cell != '':
                                        fp += 1
                                        output_row.append('FP')
                                    else:
                                        output_row.append('--')
                                elif '!' in gt_cell or not any(c.isalpha() for c in gt_cell):
                                    output_row.append('**')
                                    continue
                                else:
                                    if pred_cell == '':
                                        fn += 1
                                        output_row.append('FN')
                                    else:
                                        tp += 1
                                        output_row.append('TP')
                            output_mask_csv.writerow(output_row)
                        except StopIteration:
                            break

        print_PRF(gt_path.name, gt_tables_count[gt_path], tp, fp, fn)

        TABLES += gt_tables_count[gt_path]
        TP, FP, FN = TP + tp, FP + fp, FN + fn

        if gt_tables_count[gt_path] > 1:
            M_TABLES += gt_tables_count[gt_path]
            M_TP, M_FP, M_FN = M_TP + tp, M_FP + fp, M_FN + fn
        else:
            S_TABLES += gt_tables_count[gt_path]
            S_TP, S_FP, S_FN = S_TP + tp, S_FP + fp, S_FN + fn

    print(f"{'='*80},{'='*7},{'='*9},{'='*9},{'='*9},{'='*9},{'='*9},{'='*9}")
    print_PRF('<OVERALL>', TABLES, TP, FP, FN)
    print_PRF('<OVERALL::SINGLE_TABLE_SHEETS>', S_TABLES, S_TP, S_FP, S_FN)
    print_PRF('<OVERALL::MULTI_TABLE_SHEETS>', M_TABLES, M_TP, M_FP, M_FN)