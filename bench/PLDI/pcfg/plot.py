import matplotlib.pyplot as plt
import argparse
import csv

def times(file_name, col):
    with open(file_name, 'r') as file:
        array = csv.reader(file, delimiter=",")
        rows = iter(array)
        next(rows)
        data = [float(row[col]) for row in rows]
        return data

parser = argparse.ArgumentParser()
parser.add_argument("--ENUM", type=str, required=True)
parser.add_argument("--BASE", type=str, required=True)
parser.add_argument("--MAPPL", type=str, required=True)
parser.add_argument("--PERPL", type=str, required=True)
parser.add_argument("--DICE", type=str, required=True)
parser.add_argument("--prob", type=str, required=True)
args = parser.parse_args()

enum_times = times(args.ENUM, 1)
base_times = times(args.BASE, 1)
mappl_times = times(args.MAPPL, 1)
perpl_times = times(args.PERPL, 1)
dice_times = times(args.DICE, 1)
plt.title(f"PCFG (p={args.prob})")
plt.xlabel("Length of sentence")
plt.ylabel("Time (s)")
plt.plot(range(1, len(base_times) + 1), base_times, '+-k', label="MAPPL (base)")
plt.plot(range(1, len(enum_times) + 1), enum_times, 'o-y', label="ENUM")
plt.plot(range(1, len(mappl_times) + 1), mappl_times, '|-r', label="MAPPL")
plt.plot(range(1, len(perpl_times) + 1), perpl_times, 'o-', label="PERPL", color="purple", fillstyle='none')
plt.plot(range(1, len(dice_times) + 1), dice_times, 'x-g', label="DICE")
plt.legend()
plt.yscale("log")
plt.savefig(f"pcfg_{args.prob}.png")
