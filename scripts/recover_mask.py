#!/usr/bin/env python3

import argparse
import csv
import logging
import subprocess

from pathlib import Path
from shutil import copy2 as copyfile
from tempfile import mkstemp
from time import time_ns

ROOT_PATH = Path(__file__).parent.parent.absolute()

logging.basicConfig(format='%(asctime)s  (%(levelname)8s)  %(message)s', level=logging.DEBUG)

def dir_path(string):
    path = Path(string)
    if path.is_dir(): return path
    raise NotADirectoryError(string)

if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('-e', '--eval-csv-dir', type=dir_path, required=True)
    parser.add_argument('-o', '--output-dir', type=dir_path, required=True)
    parser.add_argument('-c', '--table-range-column', type=int, required=True)
    parser.add_argument('-f', '--filtered-files-path', type=argparse.FileType('r'), required=False)
    parser.add_argument('tables_data_csv', type=argparse.FileType('r'), nargs=1)
    args = parser.parse_args()

    tables_csv_reader = csv.reader(args.tables_data_csv[0])
    next(tables_csv_reader)

    todo = dict()
    if args.table_range_column < 0:
        for i, row in enumerate(tables_csv_reader):
            e_csv_path = args.eval_csv_dir.joinpath(row[0].strip())
            if not e_csv_path.is_file():
                logging.warning(f'Skipped missing file "{e_csv_path.name}"')
                continue
            todo[e_csv_path] = {'FULL_SHEET'}
    else:
        for i, row in enumerate(tables_csv_reader):
            e_csv_path = args.eval_csv_dir.joinpath(row[0].strip())
            if not e_csv_path.is_file():
                logging.warning(f'Skipped missing file "{e_csv_path.name}"')
                continue

            if e_csv_path not in todo:
                todo[e_csv_path] = set()

            table = row[args.table_range_column].strip()
            if not table or table == '<null>':
                logging.warning(f'Skipped an empty range in "{e_csv_path.name}" -- see line {i}')
                continue

            todo[e_csv_path].add(table)

    if args.filtered_files_path is not None:
        filtered_files = [s.strip() for s in args.filtered_files_path.readlines()]
        todo = {k:v for k,v in todo.items() if k.name in filtered_files}

    logging.info('')
    _, tmp_r_path = mkstemp()
    _, tmp_w_path = mkstemp()
    total = len(todo)

    try:
        logging.info(f'Starting recovery with. TEMP_R_FILE = "{tmp_r_path}" TEMP_W_FILE = "{tmp_w_path}"')
        logging.info('')

        for t, (e_csv_path, data) in enumerate(todo.items()):
            fm_out_path = args.output_dir.joinpath(e_csv_path.name)

            logging.info(f'@ {t+1:03} / {total:03} : "{e_csv_path.name}"')
            logging.info(f' `-- Writing to "{fm_out_path}"')

            if not data:
                logging.warning(f' `-- No tables detected!')
                cmdline = f'dune exec bin/App.exe -- -range "A1:A1" "{e_csv_path}"'
                logging.debug(f'     $ {cmdline}')
                try:
                    with open(tmp_w_path, 'w') as tmp_w_file:
                        result = subprocess.run(cmdline, cwd=ROOT_PATH, shell=True,
                                                stdout=tmp_w_file, stderr=subprocess.PIPE,
                                                universal_newlines=True)
                    if result.returncode != 0:
                        logging.error(f'      `-- Failed to generate empty formula mask!')
                        logging.error(result.stderr)
                    else:
                        copyfile(tmp_w_path, fm_out_path)
                except Exception as e:
                    logging.error(f'      `-- Failed to recover the formula mask!')
                    logging.exception(e)
                with open(f'{fm_out_path}.time', 'w') as time_file:
                    time_file.write('inf')
                continue

            start_time = time_ns()
            for i, table in enumerate(data):
                logging.info(f' `-- Inspecting table {i+1:2d} of {len(data):2d} :: [{table}] ...')
                if table == 'FULL_SHEET':
                    cmdline = f'dune exec bin/App.exe "{e_csv_path}"'
                else:
                    mask = '' if i < 1 else f'-mask "{tmp_r_path}"'
                    cmdline = f'dune exec bin/App.exe -- -range "{table}" {mask} "{e_csv_path}"'
                logging.debug(f'     $ {cmdline}')

                try:
                    with open(tmp_w_path, 'w') as tmp_w_file:
                        result = subprocess.run(cmdline, cwd=ROOT_PATH, shell=True,
                                                stdout=tmp_w_file, stderr=subprocess.PIPE,
                                                universal_newlines=True)
                    if result.returncode != 0:
                        logging.error(f'      `-- Failed to recover the formula mask!')
                        logging.error(result.stderr)
                    else:
                        copyfile(tmp_w_path, fm_out_path)
                        copyfile(tmp_w_path, tmp_r_path)
                except Exception as e:
                    logging.error(f'      `-- Failed to recover the formula mask!')
                    logging.exception(e)
            end_time = time_ns()
            with open(f'{fm_out_path}.time', 'w') as time_file:
                time_file.write(f'{(end_time - start_time)/1e9:.2f}')
            logging.info('')
    finally:
        Path(tmp_r_path).unlink()
        Path(tmp_w_path).unlink()