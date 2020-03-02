'''
Tail multiple log files and label their lines.

'''

from contextlib import contextmanager, ExitStack
from dataclasses import dataclass
from urllib.parse import urlparse, parse_qs
import click
import select
import subprocess


@click.command()
@click.argument('logpath', nargs=-1)
def main(logpath):
    '''
    Tail LOGPATH(s) and label their lines of each log in one output stream.

    Each path can be configured using query arguments, e.g.:

      \b
      multilog.py \\
          logs/test.log \\
          logs/testconfigured.log?label=configured&color=red

    The availalbe configuration options for each path are:

    label:

        The label to prepend to each line. Defaults to the name of the log file
        without extension.

    color:

        Color of the label, defaults to white or light gray.

        Available colors: black (might be a gray), red, green, yellow (might be
        an orange), blue, magenta, cyan, white (might be light gray),
        bright_black, bright_red, bright_green, bright_yellow, bright_blue,
        bright_magenta, bright_cyan, bright_white

    '''

    poll = select.poll()

    logfiles = {}
    pipes = {}

    with ExitStack() as stack:
        for path in logpath:
            logfile = parse_path(path)
            pipe = stack.enter_context(tail(logfile.path))
            logfiles[pipe.fileno()] = logfile
            pipes[pipe.fileno()] = pipe
            poll.register(pipe, select.POLLIN)

        if not logfiles:
            # Wait indefinitely
            poll.poll()

        maxlabel = max(len(logfile.label) for logfile in logfiles.values())

        while True:
            fds = poll.poll()

            for fd, _ in fds:
                logfile = logfiles[fd]
                label = (logfile.label + ":").ljust(maxlabel + 2)

                click.secho(label, fg=logfile.color or 'white', nl=False)
                click.echo(pipes[fd].readline(), nl=False)


@contextmanager
def tail(path):
    'Returns a file-like object that tails the file at the given path.'
    with subprocess.Popen(['tail', '-F', '--lines=0', path], stdout=subprocess.PIPE) as process:
        yield process.stdout


@dataclass
class LogFile:
    'Configuration for a log file.'
    path: str
    color: str
    label: str


def parse_path(path):
    'Parse a log path with optional configuration in query arguments.'
    result = urlparse(path)
    query = parse_qs(result.query)

    if 'color' in query:
        color = query['color'][-1]
    else:
        color = None

    if 'label' in query:
        label = query['label'][-1]
    else:
        label = result.path.split('/')[-1].split('.')[0]

    return LogFile(path=result.path, color=color, label=label)


if __name__ == '__main__':
    main()
