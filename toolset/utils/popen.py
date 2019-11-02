import subprocess
import time
import select
import os

class PopenTimeout(subprocess.Popen):
    '''
    Popen class with timeout for Python 2.7
    see https://stackoverflow.com/questions/1191374/using-module-subprocess-with-timeout
    '''
    def communicate(self, input=None, timeout=None):
        if timeout is None:
            return subprocess.Popen.communicate(self, input)

        if self.stdin:
            # Flush stdio buffer, this might block if user
            # has been writing to .stdin in an uncontrolled
            # fashion.
            self.stdin.flush()
            if not input:
                self.stdin.close()

        read_set, write_set = [], []
        stdout = stderr = None

        if self.stdin and input:
            write_set.append(self.stdin)
        if self.stdout:
            read_set.append(self.stdout)
            stdout = []
        if self.stderr:
            read_set.append(self.stderr)
            stderr = []

        input_offset = 0
        deadline = time.time() + timeout

        while read_set or write_set:
            try:
                rlist, wlist, xlist = select.select(read_set, write_set, [], max(0, deadline - time.time()))
            except select.error as ex:
                if ex.args[0] == errno.EINTR:
                    continue
                raise

            if not (rlist or wlist):
                # Just break if timeout
                # Since we do not close stdout/stderr/stdin, we can call
                # communicate() several times reading data by smaller pieces.
                break

            if self.stdin in wlist:
                chunk = input[input_offset:input_offset + subprocess._PIPE_BUF]
                try:
                    bytes_written = os.write(self.stdin.fileno(), chunk)
                except OSError as ex:
                    if ex.errno == errno.EPIPE:
                        self.stdin.close()
                        write_set.remove(self.stdin)
                    else:
                        raise
                else:
                    input_offset += bytes_written
                    if input_offset >= len(input):
                        self.stdin.close()
                        write_set.remove(self.stdin)

            # Read stdout / stderr by 1024 bytes
            for fn, tgt in (
                (self.stdout, stdout),
                (self.stderr, stderr),
            ):
                if fn in rlist:
                    data = os.read(fn.fileno(), 1024)
                    if data == '':
                        fn.close()
                        read_set.remove(fn)
                    tgt.append(data)

        if stdout is not None:
            stdout = ''.join(stdout)
        if stderr is not None:
            stderr = ''.join(stderr)

        return (stdout, stderr)
