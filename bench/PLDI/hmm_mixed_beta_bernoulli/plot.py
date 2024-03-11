import matplotlib.pyplot as plt
import argparse
import csv

def column(file_name, col):
    with open(file_name, 'r') as file:
        array = csv.reader(file, delimiter=",")
        rows = iter(array)
        data = [float(row[col]) for row in rows]
        return data

parser = argparse.ArgumentParser()
parser.add_argument("--IS", type=str, required=True)
parser.add_argument("--MAPPL", type=str, required=True)
parser.add_argument("--EXACT", type=str, required=True)
parser.add_argument("--horizon", type=str, required=True)
args = parser.parse_args()
is_me = column(args.IS, 4)
mappl_me = column(args.MAPPL, 4)
exact = column(args.EXACT, 4)
is_times = column(args.IS, 3)
mappl_times = column(args.MAPPL, 3)
plt.title(f"Beta-Bern, |data| = {args.horizon}")
plt.xlabel("Time (s)")
plt.ylabel("log evidence estimate")
plt.plot(is_times, is_me, 'o-k', label="IS")
plt.plot(mappl_times, mappl_me, '|-r', label="MAPPL/IS")
plt.axhline(exact, label="MAPPL/exact", color="blue")
plt.legend()
plt.yscale("log")
plt.xscale("log")
plt.savefig(f"beta-bern_{args.horizon}.png")
