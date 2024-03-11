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
parser.add_argument("--MAPPL", type=str, required=True)
parser.add_argument("--PERPL", type=str, required=True)
args = parser.parse_args()

enum_times = times(args.ENUM, 1)
mappl_times = times(args.MAPPL, 1)
perpl_times = times(args.PERPL, 1)
plt.title("HMM2")
plt.xlabel("Horizon")
plt.ylabel("Time (s)")
plt.plot(range(1, len(enum_times) + 1), enum_times, 'o-y', label="ENUM")
plt.plot(range(1, len(mappl_times) + 1), mappl_times, '|-r', label="MAPPL")
plt.plot(range(1, len(perpl_times) + 1), perpl_times, 'o-', label="PERPL", color="purple", fillstyle='none')
plt.legend()
plt.yscale("log")
plt.savefig("hmm2.png")
