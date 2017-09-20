from num2words import num2words
from functools import reduce


def number_letters(n):
    return len(num2words(n).replace(' ', '').replace('-', ''))


def main():
    def fn(acc, n):
        return acc + number_letters(n)
    return reduce(fn, range(1, 1001), 0)


if __name__ == "__main__":
    print(main())
