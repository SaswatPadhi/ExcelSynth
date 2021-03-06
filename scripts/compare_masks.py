#!/usr/bin/env python3

import argparse
import csv
import re

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

def print_stats(label, tables, time_s, cells, tp, fp, fn, atp, afp, afn):
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

    if atp + afp > 0:
        a_precision = atp / (atp + afp)
        a_precision_str = f'{a_precision:5.3f}'
    else:
        a_precision = -1
        a_precision_str = '  X  '
    if atp + afn > 0:
        a_recall = atp / (atp + afn)
        a_recall_str = f'{a_recall:5.3f}'
    else:
        a_recall = -1
        a_recall_str = '  X  '

    print(f'{label:80},  {tables:3}  , {time_s:7.1f} , {cells:7d} ,'
          f'  {tp:5d}  ,  {atp:5d}  ,  {fp:5d}  ,  {afp:5d}  ,  {fn:5d}  ,  {afn:5d}  ,'
          f'  {precision_str}  ,  {a_precision_str}  ,  {recall_str}  ,  {a_recall_str}  ', end=',')

    if precision + recall > 0:
        fscore = (2 * precision * recall) / (precision + recall)
        if a_precision + a_recall > 0:
            a_fscore = (2 * a_precision * a_recall) / (a_precision + a_recall)
            print(f'  {fscore:5.3f}  ,  {a_fscore:5.3f}  ')
        else:
            print(f'  {fscore:5.3f}  ,    X    ')
    else:
        if a_precision + a_recall > 0:
            a_fscore = (2 * a_precision * a_recall) / (a_precision + a_recall)
            print(f'    X    ,  {a_fscore:5.3f}  ')
        else:
            print(f'    X    ,    X    ')

    return (precision, recall, a_precision, a_recall)

EXCEL_VAR_REGEX = re.compile(r'[A-Z]+[0-9]+')

class Symbol:
    IGNORED = '**'
    OUT_OF_TABLE = 'XX'
    TRUE_POSITIVE = 'TP'
    TRUE_NEGATIVE = '--'
    FALSE_POSITIVE = 'FP'
    FALSE_NEGATIVE = 'FN'

if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('-g', '--ground-truth-dir', type=dir_path, required=True)
    parser.add_argument('-p', '--prediction-dir', type=dir_path, required=True)
    parser.add_argument('-o', '--output-dir', type=dir_path, required=True)
    parser.add_argument('-r', '--ground-truth-table-column', type=int, required=True)
    parser.add_argument('-t', '--tables-data-csv', type=argparse.FileType('r'), required=True)
    parser.add_argument('-c', '--table-range-column', type=int, required=False)
    args = parser.parse_args()

    gt_tables_count, todo = dict(), dict()
    for row in csv.reader(args.tables_data_csv):
        gt_path = args.ground_truth_dir.joinpath(row[0].strip())
        if not gt_path.is_file():
            continue

        if gt_path not in gt_tables_count:
            gt_tables_count[gt_path] = 0

        gt_table = row[args.ground_truth_table_column].strip()
        if gt_table and gt_table != '<null>':
            gt_tables_count[gt_path] += 1

        if gt_path not in todo:
            todo[gt_path] = set()

        if args.table_range_column is not None:
            table = row[args.table_range_column].strip()
            if not table or table == '<null>':
                continue

            todo[gt_path].add(cr_range_to_tuple(table))
        else:
            todo[gt_path] = None

    gt_tables_count = {path:table_count for path, table_count in gt_tables_count.items() if table_count > 0}
    todo = {path:tables for path, tables in todo.items() if path in gt_tables_count}

    TABLES, S_TABLES, M_TABLES = 0, 0, 0
    for _, table_count in gt_tables_count.items():
        TABLES += table_count
        if table_count > 1:
            M_TABLES += table_count
        else:
            S_TABLES += table_count

    CELLS, S_CELLS, M_CELLS = 0, 0, 0
    TIME_S, S_TIME_S, M_TIME_S = 0, 0, 0
    TP, FP, FN, A_TP, A_FP, A_FN = 0, 0, 0, 0, 0, 0
    S_TP, S_FP, S_FN, A_S_TP, A_S_FP, A_S_FN = 0, 0, 0, 0, 0, 0
    M_TP, M_FP, M_FN, A_M_TP, A_M_FP, A_M_FN = 0, 0, 0, 0, 0, 0

    print(f'{"FILENAME":80}, TABLES, TIME_SEC,NUM_CELLS, TRUE_POS, ADJ_T_P ,FALSE_POS, ADJ_F_P ,FALSE_NEG, ADJ_F_N ,PRECISION, ADJ_PREC,  RECALL , ADJ_REC , F1-SCORE, ADJ_F-1 ')
    print(f'''{'='*80},{'='*7},{'='*9},{'='*9}{f",{'-'*9},{'~'*9}"*6}''')

    for gt_path, tables in todo.items():
        pred_path = args.prediction_dir.joinpath(gt_path.name)
        if not pred_path.is_file():
            print(f'{gt_path.name:80},  {gt_tables_count[gt_path]:3}  {",  ? ? ?  "*14}')
            continue

        with open(gt_path, 'r') as gt_file:
            gt_data = list(csv.reader(gt_file))
        with open(pred_path, 'r') as pred_file:
            pred_data = list(csv.reader(pred_file))

        if len(gt_data) != len(pred_data) or any(len(g) != len(p) for (g,p) in zip(gt_data,pred_data)):
            print(f'{gt_path.name:80},  {gt_tables_count[gt_path]:3}  {",  ERROR  "*14}')
            continue

        out_data = [['' for gt_cell in gt_row] for gt_row in gt_data]

        cell_count = len(out_data) * len(out_data[0])
        CELLS += cell_count
        if gt_tables_count[gt_path] > 1:
            M_CELLS += cell_count
        else:
            S_CELLS += cell_count

        time_s = 0.0
        try:
            with open(f'{pred_path}.time', 'r') as time_file:
                time_s = float(time_file.readlines()[0].strip())
            if time_s != float('inf'):
                TIME_S += time_s
                if gt_tables_count[gt_path] > 1:
                    M_TIME_S += time_s
                else:
                    S_TIME_S += time_s
        except Exception:
            time = float('inf')

        def dependencies(c,r,seen):
            seen.add((c,r))
            vars = EXCEL_VAR_REGEX.findall(pred_data[r][c])
            vars_cr = [cr_to_tuple(var) for var in vars]
            vars_deps = [dependencies(var_c,var_r,seen) for var_c,var_r in vars_cr if (var_c,var_r) not in seen]
            return vars_cr + [dep for var_deps in vars_deps for dep in var_deps]

        with open(args.output_dir.joinpath(gt_path.name), 'w') as output_mask_file:
            output_mask_csv = csv.writer(output_mask_file)

            for r, (gt_row, pred_row) in enumerate(zip(gt_data, pred_data)):
                for c, (gt_cell, pred_cell) in enumerate(zip(gt_row, pred_row)):
                    if tables is not None and not any(contains(c, r, *t) for t in tables):
                        out_data[r][c] = Symbol.OUT_OF_TABLE
                        continue

                    if gt_cell == '':
                        if pred_cell != '':
                            out_data[r][c] = Symbol.FALSE_POSITIVE
                            for var_c, var_r in dependencies(c,r,set()):
                                if out_data[var_r][var_c][-2:] == Symbol.FALSE_NEGATIVE:
                                    out_data[r][c] = f'{Symbol.FALSE_POSITIVE}->{Symbol.TRUE_POSITIVE}'
                                    out_data[var_r][var_c] = f'{Symbol.FALSE_NEGATIVE}->{Symbol.TRUE_POSITIVE}'
                        else:
                            out_data[r][c] = Symbol.TRUE_NEGATIVE
                    elif '!' in gt_cell or not any(c.isalpha() for c in gt_cell):
                        out_data[r][c] = Symbol.IGNORED
                        continue
                    else:
                        if pred_cell == '':
                            out_data[r][c] = Symbol.FALSE_NEGATIVE
                        else:
                            out_data[r][c] = Symbol.TRUE_POSITIVE

            tp, fp, fn, atp, afp, afn = 0, 0, 0, 0, 0, 0
            for r, out_row in enumerate(out_data):
                for c, cell in enumerate(out_row):
                    if cell[-2:] == Symbol.TRUE_POSITIVE: atp += 1
                    if cell[-2:] == Symbol.FALSE_POSITIVE: afp += 1
                    if cell[-2:] == Symbol.FALSE_NEGATIVE: afn += 1
                    if cell[:2] == Symbol.TRUE_POSITIVE: tp += 1
                    if cell[:2] == Symbol.FALSE_POSITIVE: fp += 1
                    if cell[:2] == Symbol.FALSE_NEGATIVE: fn += 1
                    out_data[r][c] = f'{cell:^6}'
            output_mask_csv.writerows(out_data)

        print_stats(gt_path.name, gt_tables_count[gt_path], time_s, cell_count, tp, fp, fn, atp, afp, afn)

        TP, FP, FN = TP + tp, FP + fp, FN + fn
        A_TP, A_FP, A_FN = A_TP + atp, A_FP + afp, A_FN + afn

        if gt_tables_count[gt_path] > 1:
            M_TP, M_FP, M_FN = M_TP + tp, M_FP + fp, M_FN + fn
            A_M_TP, A_M_FP, A_M_FN = A_M_TP + atp, A_M_FP + afp, A_M_FN + afn
        else:
            S_TP, S_FP, S_FN = S_TP + tp, S_FP + fp, S_FN + fn
            A_S_TP, A_S_FP, A_S_FN = A_S_TP + atp, A_S_FP + afp, A_S_FN + afn

    print(f'''{'='*80},{'='*7},{'='*9},{'='*9}{f",{'-'*9},{'~'*9}"*6}''')
    print_stats('MICRO_AGG', TABLES, TIME_S, CELLS, TP, FP, FN, A_TP, A_FP, A_FN)
    print_stats('MICRO_AGG::SINGLE_TABLE_SHEETS', S_TABLES, S_TIME_S, S_CELLS, S_TP, S_FP, S_FN, A_S_TP, A_S_FP, A_S_FN)
    print_stats('MICRO_AGG::MULTI_TABLE_SHEETS', M_TABLES, M_TIME_S, M_CELLS, M_TP, M_FP, M_FN, A_M_TP, A_M_FP, A_M_FN)