#!/usr/bin/env python3
# messy script for generating lox tokens to scan
# usage: gen-tokens.py AMOUNT

import sys
import string
import random
random.seed(2137)

tokens = [
    '(',
    ')',
    '{',
    '}',
    '.',
    ',',
    '-',
    '+',
    ';',
    '/',
    '*',
    '!',
    '!=',
    '=',
    '==',
    '<',
    '<=',
    '>',
    '>=',
    "and",
    "class",
    "else",
    "false",
    "fun",
    "for",
    "if",
    "nil",
    "or",
    "print",
    "return",
    "super",
    "this",
    "true",
    "var",
    "while",
    "i", # identifier
    "s", # string
    "n", # number
]

ident_alphabet = '_' + string.ascii_letters
str_alphabet = str(c for c in string.printable if c != '"')
keywords = set(t for t in tokens if len(t) > 1 and set(t) < set(string.ascii_letters))

last_was_ident = False
def choose_token():
    global last_was_ident
    token = random.choice(tokens)
    match token:
        case 'i':
            ident_len = random.binomialvariate(n=20) + 1
            name = ''.join(random.choices(ident_alphabet, k=ident_len))
            if last_was_ident:
                return f" {name}"
            else:
                last_was_ident = True
                return name
        case 's':
            last_was_ident = False
            str_len = random.binomialvariate(n=20)
            return f"\"{''.join(random.choices(str_alphabet, k=str_len))}\""
        case 'n':
            last_was_ident = False
            if random.randrange(2):
                return str(random.randint(-2**32, 2**32))
            else:
                return str(random.uniform(-2**32, 2**32))
        case _:
            if token in keywords:
                if last_was_ident:
                    last_was_ident = True
                    return f" {token}"
                last_was_ident = True
            else:
                last_was_ident = False
            return token

for _ in range(int(sys.argv[1])):
    print(choose_token(), end='')
