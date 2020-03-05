import argparse
import csv

from pathlib import Path
from statistics import mean

def dir_path(string):
    path = Path(string)
    if path.is_dir(): return path
    raise NotADirectoryError(string)

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument('-g', '--ground-truth-dir', type=dir_path, required=True)
    parser.add_argument('-p', '--prediction-dir', type=dir_path, required=True)
    parser.add_argument('-o', '--false-positive-output-dir', type=dir_path, required=True)
    args = parser.parse_args()

    ap , ar , af = [], [], []

    print(f'{"FILENAME":80},PRECISION,   RECALL, F1-SCORE')
    print(f"{'='*80},{'='*9},{'='*9},{'='*9}")

    for gt_path in sorted(args.ground_truth_dir.glob('*.csv')):
        pred_path = args.prediction_dir.joinpath(gt_path.name)
        if not pred_path.is_file():
            print(f'{gt_path.name:80},    -    ,    -    ,    -    ')
            continue

        tp , fp , fn = 0.0 , 0.0 , 0.0

        with open(gt_path, 'r') as gt_file:
            gt_csv = csv.reader(gt_file)

            with open(pred_path, 'r') as pred_file:
                pred_csv = csv.reader(pred_file)

                fp_mask_path = args.false_positive_output_dir.joinpath(gt_path.name)
                with open(fp_mask_path, 'w') as fp_mask_file:
                        fp_mask_csv = csv.writer(fp_mask_file)
                        fp_mask_csv.writerows(((pred_field if gt_field == "" and pred_field != "" else "")
                                               for gt_field,pred_field in zip(gt_row,pred_row))
                                              for gt_row,pred_row in zip(gt_csv, pred_csv))

                gt_file.seek(0)
                pred_file.seek(0)

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

            ap.append(precision)
            ar.append(recall)
            af.append(fscore)

            print(f'{gt_path.name:80},{precision:9.3},{recall:9.3},{fscore:9.3}')
        else:
            print(f'{gt_path.name:80},    -    ,    -    ,    -    ')

    print(f"{'='*80},{'='*9},{'='*9},{'='*9}")
    print(f'{"<AVERAGE>":80},{mean(ap):9.3},{mean(ar):9.3},{mean(af):9.3}')