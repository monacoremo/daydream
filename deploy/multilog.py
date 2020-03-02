'''
Multilog: Tail multiple logs.

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
    poll = select.poll()

    logfiles = {}
    pipes = {}

    with ExitStack() as stack:
        for path in logpath:
            logfile = parse_path(path)
            pipe = stack.enter_context(tail(logfile.path))
            logfiles[pipe.fileno()] = logfile
            pipes[pipe.fileno()] = pipe
            poll.register(pipe)

        maxlabel = max(len(logfile.label) for logfile in logfiles.values())

        while True:
            fds = poll.poll()

            for fd, event in fds:
                if event == select.POLLIN:
                    logfile = logfiles[fd]
                    label = (logfile.label + ":").ljust(maxlabel + 2)

                    click.secho(label, fg=logfile.color or 'blue', nl=False)
                    click.echo(pipes[fd].readline(), nl=False)


@contextmanager
def tail(path):
    with subprocess.Popen(['tail', '-F', '--lines=0', path], stdout=subprocess.PIPE) as process:
        yield process.stdout


@dataclass
class LogFile:
    path: str
    color: str
    label: str


def parse_path(path):
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
