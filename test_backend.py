import os
import sys
import subprocess

def test_output():
    pass

def test_exit():
    pass

def compile(f):
    pass

if __name__ == '__main__':
    filename = sys.argv[1]
    expected = {}
    infile = open(filename, 'rt')
    for line in infile:
        if line.startswith("#"):
            tks = line.split(" ")
            if tks[1].strip() in ['Output:', 'Exit']:
                title = tks[1].strip()
                expected[title] = []
                sys.stdout.write(tks[1])
                for l in infile:
                    if not l.startswith("#"):
                        sys.stdout.write(''.join(expected[title]))
                        break
                    else:
                        ll = ' '.join(l.split(' ')[1:])
                        expected[title].append(ll)
    infile.close()
