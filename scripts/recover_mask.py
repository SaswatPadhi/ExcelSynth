import argparse
import csv
import logging
import subprocess

from pathlib import Path
from shutil import copy2 as copyfile
from tempfile import mkstemp

ROOT_PATH = Path(__file__).parent.parent.absolute()

logging.basicConfig(format='%(asctime)s  (%(levelname)8s)  %(message)s', level=logging.DEBUG)

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
            logging.warning(f'Skipped missing file "{e_csv_path.name}"')
            continue

        table = row[args.table_range_column].strip()
        if not table or table == '<null>':
            logging.warning(f'Skipped an empty range in "{e_csv_path.name}" -- see line {i}')
            continue

        if e_csv_path in todo:
            todo[e_csv_path].add(table)
        else:
            todo[e_csv_path] = set([table])

    logging.info('')
    _, tmp_r_path = mkstemp()
    _, tmp_w_path = mkstemp()

    try:
        logging.info(f'Starting recovery with. TEMP_R_FILE = "{tmp_r_path}" TEMP_W_FILE = "{tmp_w_path}"')
        logging.info('')

        for e_csv_path, data in todo.items():
            fm_out_path = args.output_dir.joinpath(e_csv_path.name)
            logging.info(f'@ "{e_csv_path.name}"')
            for i, table in enumerate(data):
                logging.info(f' `-- Inspecting range [{table}] ...')
                try:
                    mask = '' if i < 1 else f'-mask "{tmp_r_path}"'
                    cmdline = f'dune exec bin/App.exe -- -range "{table}" {mask} "{e_csv_path}"'
                    logging.debug(f'     $ {cmdline}')

                    with open(tmp_w_path, 'w') as tmp_w_file:
                        result = subprocess.run(cmdline, cwd=ROOT_PATH, shell=True,
                                                stdout=tmp_w_file, stderr=subprocess.PIPE,
                                                universal_newlines=True)
                    if result.returncode != 0:
                        logging.error(f'      `-- Failed to recover the formula mask!')
                        logging.error(result.stderr)
                        exit(1)
                    else:
                        copyfile(tmp_w_path, fm_out_path)
                        copyfile(tmp_w_path, tmp_r_path)
                except Exception as e:
                    logging.error(f'      `-- Failed to recover the formula mask!')
                    logging.exception(e)
    finally:
        Path(tmp_r_path).unlink()
        Path(tmp_w_path).unlink()