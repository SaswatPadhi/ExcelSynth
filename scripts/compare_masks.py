import argparse
import csv

from pathlib import Path

def dir_path(string):
    path = Path(string)
    if path.is_dir(): return path
    raise NotADirectoryError(string)

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument('-g', '--ground-truth-dir', type=dir_path, required=True)
    parser.add_argument('-p', '--prediction-dir', type=dir_path, required=True)
    args = parser.parse_args()

    for gt_path in sorted(args.ground_truth_dir.glob('*.csv')):
        pred_path = args.prediction_dir.joinpath(gt_path.name)
        if not pred_path.is_file():
            print(f'{gt_path.name:80},--------,--------,--------')
            continue

        tp , fp , fn = 0.0 , 0.0 , 0.0

        with open(gt_path, 'r') as gt_file:
            gt_csv = csv.reader(gt_file)

            with open(pred_path, 'r') as pred_file:
                pred_csv = csv.reader(pred_file)

                while True:
                    try:
                        gt_row = next(gt_csv)
                        pred_row = next(pred_csv)
                        for gt_cell in gt_row:
                            for pred_cell in pred_row:
                                if gt_cell == "":
                                    if pred_cell != "":
                                        fp += 1
                                else:
                                    if pred_cell == "":
                                        fn += 1
                                    else:
                                        tp += 1
                    except StopIteration:
                        break

        if tp > 0 and tp + fp > 0.0 and tp + fn > 0.0:
            precision = tp / (tp + fp)
            recall = tp / (tp + fn)
            fscore = (2.0 * precision * recall) / (precision + recall)

            print(f'{gt_path.name:80},{precision:8.3},{recall:8.3},{fscore:8.3}')
        else:
            print(f'{gt_path.name:80},--------,--------,--------')