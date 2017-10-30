import os
import sys
import subprocess

def compile(f):
    subprocess.run(['./wacc/env.byte', f])
    subprocess.run(['./tools/arm-gcc', f.replace('.wacc', '.s')])
    out = subprocess.run(['./tools/arm-run', 'a.out'])
    output = '' if not out.stdout else out.stdout.decode('utf-8')
    return (output, out.returncode)

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
            assert expected_output == output

    if "Exit:" in expected:
        expected_ret = int(''.join(expected['Exit:']).strip())
        assert expected_ret == retcode


if __name__ == '__main__':
    test_run(sys.argv[1])
