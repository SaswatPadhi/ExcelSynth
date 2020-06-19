#!/usr/bin/env python3

import argparse
import csv
import logging
import numpy as np
import os

from matplotlib import pyplot as plt
from matplotlib import rcParams
from openpyxl.formula import Tokenizer
from openpyxl.formula.tokenizer import Token
from operator import itemgetter
from pathlib import Path

logging.basicConfig(format='%(asctime)s  (%(levelname)8s)  %(message)s', level=logging.DEBUG)

rcParams['font.family'] = 'monospace'
rcParams['font.sans-serif'] = ['Lato']
rcParams['font.monospace'] = ['mononoki']

def dir_path(string):
    path = Path(string)
    if path.is_dir(): return path
    raise NotADirectoryError(string)

if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('-i', '--input-dir', type=dir_path, required=True)
    parser.add_argument('-o', '--output-dir', type=dir_path, required=True)
    parser.add_argument('-p', '--plot-output-path', type=str, required=True)
    parser.add_argument('-f', '--filtered-files-output-path', type=str, required=True)
    args = parser.parse_args()

    formula_stats, filtered_files_list = dict(), []
    for src in args.input_dir.glob('*.csv'):
        dst = args.output_dir.joinpath(src.name)
        with open(src, 'r') as src_file:
            src_csv = csv.reader(src_file)
            no_formulas, rows = True, []
            for src_row in src_csv:
                row = []
                for field in src_row:
                    has_formula = False
                    tokens = Tokenizer(field)
                    for t in tokens.items:
                        if t.type == Token.FUNC and t.subtype == Token.OPEN:
                            has_formula, no_formulas, value = True, False, t.value[:-1]
                            if value not in formula_stats:
                                formula_stats[value] = 0
                            formula_stats[value] += 1
                        elif t.type == Token.OP_PRE or t.type == Token.OP_IN or t.type == Token.OP_POST:
                            has_formula, no_formulas, value = True, False, t.value
                            if value not in formula_stats:
                                formula_stats[value] = 0
                            formula_stats[value] += 1
                        elif t.type == Token.OPERAND:
                            has_formula, no_formulas = True, False
                    row.append(field if has_formula else '')
                rows.append(row)
            if not no_formulas:
                filtered_files_list.append(src.name)
                with open(dst, 'w') as dst_file:
                    csv.writer(dst_file).writerows(rows)
            else:
                logging.warning(f'Skipped file "{src.name}" since it had no formula cells')

    with open(args.filtered_files_output_path, 'w') as filter_file:
        filter_file.write(os.linesep.join(filtered_files_list))

    sorted_formula_stats = sorted(formula_stats.items(), key=itemgetter(1))
    x_labels, y_labels = zip(*sorted_formula_stats[-10:])
    x = list(range(len(y_labels)))

    plt.figure(figsize=(6.75,9.25))
    plt.bar(x, height=y_labels, align='center', width=0.6667, zorder=10)

    plt.xticks(range(len(y_labels)), x_labels, fontsize=15)
    plt.yticks(np.arange(0, (int(max(y_labels)/2500)+2)*2500, 2500),
               np.arange(0, (int(max(y_labels)/2500)+2)*2.5, 2.5), fontsize=15)
    plt.grid(True, linestyle=':', color='#aaaaaa', zorder=0)

    plt.xlabel('Excel Function or Operator', labelpad=12,
               fontsize=20, fontfamily='sans-serif', fontweight='bold')
    plt.ylabel('Number of Occurrences in Training Dataset   (in 1000s)', labelpad=12,
               fontsize=20, fontfamily='sans-serif', fontweight='bold')

    plt.savefig(args.plot_output_path, bbox_inches='tight', dpi=256)