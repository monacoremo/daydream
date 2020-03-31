'''
Tail multiple log files and label their lines.

'''

from contextlib import contextmanager, ExitStack
from collections import namedtuple
from urllib.parse import urlparse, parse_qsl
import click
import select
import subprocess
import itertools
import fcntl
import os


LogFile = namedtuple('LogFile', 'pipe config')
LogFileConfig = namedtuple('LogFileConfig', 'path color label')


COLORS = '''
    red green yellow blue magenta cyan white black bright_red bright_green
    bright_yellow bright_blue bright_magenta bright_cyan bright_white
    bright_black
'''.strip().split()


@click.command()
@click.argument('logpath', nargs=-1)
@click.option('-l', '--list-colors', is_flag=True, default=False)
@click.option('--colorize-labels/--no-colorize-labels', default=True)
@click.option('--verbose/--quiet', default=False)
def main(logpath, list_colors=False, colorize_labels=True, verbose=False):
    '''
    Tail all LOGPATH(s), label their lines and output them in one stream.

    Each path can be configured using query arguments, e.g.:

        logmux logs/test.log configured.log?label=custom&color=red

    The availalbe configuration options for each path are:

        label: The label to prepend to each line. Defaults to the name of the
        log file without its extension.

        color: Color of the label. Add "-l" to list the available colors.

    '''


    if list_colors:
        click.echo('The following colors are available:', err=True)
        for color in COLORS:
            click.secho('    ' + color, fg=color, err=True)
        return

    logfiles = {}

    with ExitStack() as stack:
        for path in logpath:
            try:
                config = logfileconfig(path)
            except ValueError as err:
                click.echo(f'Configuration error at "{path}": {err}', err=True)
                return

            pipe = stack.enter_context(tail(config.path))
            logfiles[pipe.fileno()] = LogFile(pipe=pipe, config=config)

        if colorize_labels:
            # assign a color to each log file that had none configured
            colors = iter(itertools.cycle(COLORS))
            for key, logfile in logfiles.items():
                if not logfile.config.color:
                    config = logfile.config._replace(color=next(colors))
                    logfiles[key] = logfile._replace(config=config)

        labels = (logfile.config.label for logfile in logfiles.values())
        maxlabel = max(map(len, labels), default=1)

        poll = select.poll()
        for logfile in logfiles.values():
            poll.register(logfile.pipe)

        if verbose:
            click.echo(f'Tailing {len(logfiles)} file(s)...', err=True)

        while True:
            for fd, event in poll.poll():
                logfile = logfiles[fd]

                if event == select.EPOLLIN:
                    label = (logfile.config.label + ":").ljust(maxlabel + 2)

                    while True:
                        line = logfile.pipe.readline()
                        if not line:
                            break

                        if logfile.config.color:
                            click.secho(label, fg=logfile.config.color, nl=False)
                        else:
                            click.echo(label, nl=False)

                        click.echo(line, nl=False)

                elif verbose:
                    msg = f'Got unexpected event {event} on "{logfile.config.path}".'
                    click.echo(msg, err=True)


@contextmanager
def tail(path):
    'Returns a file-like object that tails the file at the given path.'

    command = ['stdbuf', '-oL', 'tail', '-F', '--lines=0', path]

    with subprocess.Popen(command, stdout=subprocess.PIPE) as process:
        set_nonblocking(process.stdout)
        yield process.stdout


def set_nonblocking(fd):
    'Set a file descriptior or file-like object to be non-blocking.'

    fl = fcntl.fcntl(fd, fcntl.F_GETFL)
    fcntl.fcntl(fd, fcntl.F_SETFL, fl | os.O_NONBLOCK)


def logfileconfig(path):
    'Parse a log path with optional configuration in query arguments.'

    result = urlparse(path)

    if any([result.scheme, result.netloc, result.params, result.fragment]):
        raise ValueError('Only regular files are supported.')

    label = result.path.split('/')[-1].split('.')[0]
    color = None

    for option, value in parse_qsl(result.query):
        if option == 'color':
            if value not in COLORS:
                raise ValueError(
                    f'Invalid color: "{value}".'
                    ' Add "-l" to list the available colors.'
                )
            color = value
        elif option == 'label':
            label = value
        else:
            raise ValueError(
                f'Invalid option: "{option}".'
                ' Use "label" or "color".'
            )

    return LogFileConfig(path=result.path, color=color, label=label)


if __name__ == '__main__':
    main()
