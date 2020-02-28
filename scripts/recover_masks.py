import argparse
import csv
import logging
import subprocess

from pathlib import Path
from shutil import copy2 as copyfile
from tempfile import mkstemp

ROOT_PATH = Path(__file__).parent.parent.absolute()

logging.basicConfig(format='%(asctime)s  (%(levelname)8s)  %(message)s', level=logging.INFO)

def dir_path(string):
    path = Path(string)
    if path.is_dir(): return path
    raise NotADirectoryError(string)

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument('-e', '--eval-csv-dir', type=dir_path, required=True)
    parser.add_argument('-o', '--output-dir', type=dir_path, required=True)
    parser.add_argument('-c', '--table-range-column', type=int, required=True)
    parser.add_argument('tables_data_csv', type=argparse.FileType('r'), nargs=1)
    args = parser.parse_args()

    todo = {}
    for i, row in enumerate(csv.reader(args.tables_data_csv[0])):
        e_csv_path = args.eval_csv_dir.joinpath(row[0].strip())
        if not e_csv_path.is_file():
            logging.warning(f'Skipping "{e_csv_path}": File not found!')
            continue

        table = row[args.table_range_column].strip()
        if not table:
            logging.warning(f'Skipping an empty range in "{e_csv_path}" -- see line {i}')
            continue

        if e_csv_path in todo:
            todo[e_csv_path].add(table)
        else:
            todo[e_csv_path] = set([table])

    _, tmp_path = mkstemp()
    try:
        logging.info(f'Starting recovering with "{tmp_path}" as the temp file')
        for e_csv_path, data in todo.items():
            fm_out_path = args.output_dir.joinpath(e_csv_path.name)
            logging.info(f'Recovering formula mask for "{e_csv_path}" > "{fm_out_path}"')
            for i, table in enumerate(data):
                logging.info(f' `-- Inspecting range [{table}] ...')
                try:
                    mask = '' if i < 1 else f'-mask "{tmp_path}"'
                    with open(tmp_path, 'w') as tmp_file:
                        subprocess.run(f'dune exec bin/App.exe -- -range {table} {mask} "{e_csv_path}"',
                                    stdout=tmp_file, stderr=subprocess.DEVNULL, universal_newlines=True,
                                    cwd=ROOT_PATH, shell=True, check=True)
                    copyfile(tmp_path, fm_out_path)
                except:
                    logging.error(f'     `-- Failed to recover the formula mask!')
    finally:
        Path(tmp_path).unlink()