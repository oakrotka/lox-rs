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
    "c", # comment
    'w', # whitespace
]

ident_alphabet = '_' + string.ascii_letters
ident_alphabet_set = set(ident_alphabet)

str_alphabet = str(c for c in string.printable if c != '"')

keywords = set(t for t in tokens if len(t) > 1 and set(t) < set(string.ascii_letters))

def is_ident(token):
    return token and token[-1] in ident_alphabet_set

def handle_ident(cur, prev):
    return f" {cur}" if is_ident(prev) else cur

def choose_token(prev):
    global last_token
    token = random.choice(tokens)
    match token:
        case 'i':
            ident_len = random.binomialvariate(n=20) + 1
            name = ''.join(random.choices(ident_alphabet, k=ident_len))
            return handle_ident(token, prev)
        case 's':
            str_len = random.binomialvariate(n=20)
            contents = ''.join(random.choices(str_alphabet, k=str_len))
            return f"\"{contents}\""
        case 'n':
            num_gen_fn = random.randint if random.randrange(2) else random.uniform
            return str(num_gen_fn(-2**32, 2**32))
        case 'c':
            str_len = random.binomialvariate(n=20)
            contents = ''.join(random.choices(str_alphabet, k=str_len))
            return '//' + contents + '\n'
        case 'w':
            return random.choice('\r\t\n ')
        case '/':
            return '/' if prev != '/' else ' /'
        case _:
            return handle_ident(token, prev)

last_token = None
def gen_token():
    global last_token
    new_token = choose_token(last_token)
    last_token = new_token
    return new_token

for _ in range(int(sys.argv[1])):
    print(gen_token(), end='')
