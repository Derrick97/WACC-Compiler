#!/usr/bin/env python3

import os
import os.path as path
import subprocess
import argparse

wacc_bin = './compile'


test_dir = os.path.abspath(os.path.join(os.path.dirname(__file__), 'test/wacc_examples'))
valid_file_dir = os.path.join(test_dir, 'valid')
invalid_file_dir = os.path.join(test_dir, 'invalid/syntaxErr')
semantic_error_dir = os.path.join(test_dir, 'invalid/semanticErr')

semantic_error_code = 200
syntax_error_code = 100

exit_on_first_error = False
all_passed = True
print_source = False

files_to_compile = []

parser = argparse.ArgumentParser('Test harness for the WACC compiler')
parser.add_argument('-v', action='store_true', dest='is_verbose', help='Produce verbose output')
parser.add_argument(nargs='*', action='store', dest='dir_or_files',
                    help='filenames or dirs')
parser.add_argument('--section', choices=['valid', 'semantic', 'invalid'])
parser.add_argument('--failfirst', action='store_true')
parser.add_argument('--print-source', action='store_true')

import logging

def test_valid():
    files = find_files(valid_file_dir)
    for f in files:
        compile(f, code=0)

def test_syntax_error():
    files = find_files(invalid_file_dir)
    for f in files:
        compile(f, code=syntax_error_code)

def test_semantic_err():
    files = find_files(semantic_error_dir)
    for f in files:
        compile(f, code=semantic_error_code)

def compile(filename, code=0):
    global all_passed
    logging.info("Running compiler on %s" % filename)
    try:
        output = subprocess.check_output([wacc_bin, filename],
                                         stderr=subprocess.STDOUT)
        if code != 0:
            print("Failed on {}".format(filename))
            all_passed = False
    except subprocess.CalledProcessError as e:
        if e.returncode != code:
            print("Failed on {}".format(filename))
            print("------ start of compiler output")
            print(e.output)
            print("------ end of compiler output")
            if print_source:
                print("The source wacc file is:")
                subprocess.call(['cat', '-n', filename])
            all_passed = False
            if exit_on_first_error:
                exit(-1)

def find_files(file_dir_lst):
    to_run = []
    def add_dir_files(directory):
        for dirpath, dirnames, filenames in os.walk(directory):
            for f in filenames:
                if f.endswith('.wacc'):
                    to_run.append(path.join(dirpath, f))
    if isinstance(file_dir_lst, str):
        file_dir_lst = [file_dir_lst]
    for file_or_dir in file_dir_lst:
        if path.isdir(file_or_dir):
            add_dir_files(file_or_dir)
        else:
            to_run.append(file_or_dir)
    return to_run

if __name__ == '__main__':
    args = parser.parse_args()
    level = logging.WARN
    if args.is_verbose: level = logging.INFO
    if args.failfirst: exit_on_first_error = True
    if args.print_source: print_source = True
    logging.basicConfig(level=level, format="%(message)s")
    if not args.dir_or_files:
        test_valid()
        test_syntax_error()
        test_semantic_err()
        if all_passed:
            print("All tests have passed. Have a nice day! :D")
    else:
        print("Running with selected files not supported")

