import sys
import glob


def read_fileb(filename, mode='rb'):
    f = open(filename, mode)
    try:
        return f.read()
    finally:
        f.close()


def write_fileb(filename, value, mode='wb'):
    f = open(filename, mode)
    try:
        f.write(value)
    finally:
        f.close()

for filename in glob.glob(sys.argv[1]):
    data1 = read_fileb(filename)
    write_fileb(filename + '.bak2', data1)
    data2lines = read_fileb(filename).strip().split('\n')
    data2 = '\n'.join([line.rstrip(
    ).replace('\t', '    ' * 2) for line in data2lines]) + '\n'
    write_fileb(filename, data2)
    print filename, len(data1) - len(data2)
