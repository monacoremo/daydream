"Utility for finding random free ports."

import socket


def randomfreeport():
    "Return a random free port."

    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    s.bind(('', 0))
    addr = s.getsockname()
    s.close()
    return addr[1]


if __name__ == "__main__":
    print(randomfreeport())
