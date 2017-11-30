#!/usr/bin/env python3
import os
import sys
import subprocess
import argparse

def compile(f):
    try:
        subprocess.run(['./compile', f], timeout=5)
        subprocess.run(['./tools/arm-gcc', os.path.basename(f).replace('.wacc', '.s')])
        out = subprocess.run(['./tools/arm-run', 'a.out'], stdout=subprocess.PIPE, timeout=5)
        output = '' if not out.stdout else out.stdout.decode('utf-8')
        return (output, out.returncode)
    except subprocess.TimeoutExpired:
        return ("timeout", -1)
    except UnicodeDecodeError:
        return ("failed", -1)

def test_run(filename):
    expected = {}
    infile = open(filename, 'rt')
    for line in infile:
        if line.startswith("#"):
            tks = line.split(" ")
            if tks[1].strip() in ['Output:', 'Exit:']:
                title = tks[1].strip()
                expected[title] = []
                for l in infile:
                    if not l.startswith("#"):
                        break
                    else:
                        ll = ' '.join(l.split(' ')[1:])
                        expected[title].append(ll)
    infile.close()
    output, retcode = compile(filename)
    if "Output:" in expected:
        expected_output = ''.join(expected['Output:'])
        if "#empty#" in expected_output:
            assert len(output) == 0
        else:
            if expected_output != output:
                print("expected")
                print(expected_output)
                print('but got')
                print(output)

    if "Exit:" in expected:
        expected_ret = int(''.join(expected['Exit:']).strip())
        if not expected_ret == retcode:
            print("FAILED")
    else:
        if not retcode == 0:
            print("FAILED non zero return")

with open("excluded", 'r') as excluded:
    excluded = excluded.read().split("\n")

excluded = [os.path.abspath(e) for e in excluded if len(e) > 0]


def iter_tests(path="./testsuite/valid"):
    for (root, dirs, files) in os.walk(path):
        for name in files:
            p = os.path.abspath(os.path.join(root, name))
            if p.endswith(".wacc") and p not in excluded:
                print(p)
                test_run(p)

if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument("-d", "--dir", default="./testsuite/valid")
    args = parser.parse_args()
    iter_tests(args.dir)
