def collatz(n):
    if (n != 1):
        print n, "-"
        if (n % 2 == 0):
            return collatz(n/2)
        else:
            return collatz(3*n+1)
    else:
        print n
        return n

collatz(397)
