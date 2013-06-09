#!/usr/bin/env python
# coding: utf-8
import sys
import csv
import numpy
import matplotlib.pyplot as plt


def parse_entries(entries):
    result = {}

    for entry in entries:
        operation, times = entry[0], [float(t) for t in entry[1:]]
        group, op = operation.split('/')
        kind, threads = group.split()
        result.setdefault(kind, {}).setdefault(int(threads), {})[op] = times

    return result


def destructive_vs_pure(measurements, op, nthreads):
    bar_width = 0.2
    bar_gap = 0.05 # space between bars for equal x

    threads = range(1, nthreads + 1)

    destructive_medians = [measurements['destructive'][i][op][0] for i in threads]
    destructive_stddev = [measurements['destructive'][i][op][3] for i in threads]
    left = numpy.arange(1, nthreads + 1)
    destructive = plt.bar(left=left - bar_width/2 - bar_gap, height=destructive_medians, width=bar_width, yerr=destructive_stddev, ecolor='k', align='center', hatch='x', color='none')

    pure_medians = [measurements['pure'][i][op][0] for i in threads]
    pure_stddev = [measurements['pure'][i][op][3] for i in threads]
    pure = plt.bar(left=left + bar_width/2 + bar_gap, height=pure_medians, width=bar_width, yerr=pure_stddev, ecolor='k', align='center', color='none')

    plt.title(op)
    plt.xlabel('$p$')
    plt.ylabel('$t$ (s)')
    plt.legend((destructive[0], pure[0]), ('Data.Concurrent.OrderedSet', 'Data.Set'))
    plt.xticks(threads)

    plt.savefig('{}-comparison.eps'.format(op))
    plt.close()


def scalability(measurements, op, nthreads):
    threads = range(1, nthreads + 1)

    destructive_medians = [measurements['destructive'][i][op][0] for i in threads]
    destructive_speedup = [destructive_medians[0] / destructive_medians[i] for i in range(nthreads)]
    destructive = plt.plot(threads, destructive_speedup, marker='^', linestyle='solid', linewidth=1.0, color='black')

    pure_medians = [measurements['pure'][i][op][0] for i in threads]
    pure_speedup = [pure_medians[0] / pure_medians[i] for i in range(nthreads)]
    pure = plt.plot(threads, pure_speedup, marker='s', linestyle='dashed', linewidth=1.0, color='black')

    plt.title(op)
    plt.xlabel(u'$p$')
    plt.ylabel(u'$S_p$')
    plt.legend((destructive[0], pure[0]), ('Data.Concurrent.OrderedSet', 'Data.Set'))

    plt.savefig('{}-scalability.eps'.format(op))
    plt.close()


def main(summary_file_name):
    nthreads = 4
    ops = ('insert', 'contains', 'delete')

    with open(summary_file_name, 'rb') as data:
        reader = csv.reader(data)
        rows = list(reader)
        header, entries = rows[0], rows[1:]
        measurements = parse_entries(entries)
        for op in ops:
            destructive_vs_pure(measurements, op, nthreads)
        for op in ops:
            scalability(measurements, op, nthreads)

    return 0


if __name__ == '__main__':
    args = sys.argv[1:]

    if len(args) != 1:
        print('usage: {} SUMMARY.CSV'.format(sys.argv[0]))
        sys.exit(1)
    summary_file_name = args[0]

    sys.exit(main(summary_file_name))
