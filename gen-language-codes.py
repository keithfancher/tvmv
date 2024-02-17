#!/usr/bin/env python3


# Generates a Haskell module which exports two sets: the 2-letter ISO-639
# language codes and the 3-letter ISO-639 language codes.
#
# Outputs the module to stdout, assuming you'll pipe it through your favorite
# code formatter and write the results to a file somewhere.
#
# Uses data from:
# https://www.loc.gov/standards/iso639-2/php/English_list.php
# ...which appears to be the source of truth for these?!
#
# Similar in spirit to something like:
# https://github.com/HugoDaniel/iso639
# ...but WAY simpler. (And ours generates both 2- and 3-letter codes. The above
# only generates the 2-letter codes.)
#
# Our code doesn't need to build real data types, enums, and so on -- we only
# need the ability to validate a given code. We don't even care what language
# it's for!
#
# Depends on Beautiful Soup 4 and Requests.
#     sudo apt install python3-bs4 python3-requests
# Formatted with Black.
#     pip3 install black


from bs4 import BeautifulSoup
import datetime
import requests


URL = "https://www.loc.gov/standards/iso639-2/php/English_list.php"
MODULE_NAME = "Language.Codes"


def codes_from_row(row):
    tds = row.find_all("td")
    (two_let_codes, three_let_codes) = ([], [])
    if tds:
        two_let_codes = split_codes(tds[-1].string)
        three_let_codes = split_codes(tds[-2].string)
    return (two_let_codes, three_let_codes)


def split_codes(code_string):
    # Codes can be in the form "ab" or "ab/cd". In other words, there can be
    # multiple "synonyms" for a language in a single `td` element.
    codes = []
    code = code_string.strip()
    if code:
        codes = code.split("/")
    return codes


def find_codes_table(soup):
    # Of course there's no `id` to grab, so a looser check:
    th = soup.find("th", string="English Name of Language")
    return th.parent.parent


def haskell_module(two_let_codes, three_let_codes):
    gen_ts = datetime.datetime.now()
    two_let_list = to_haskell_list(two_let_codes)
    three_let_list = to_haskell_list(three_let_codes)
    module = f"""module {MODULE_NAME} (iso639_1, iso639_2) where

-- GENERATED CODE, DO NOT EDIT!
-- Generated on {gen_ts}
-- from {URL}

import Data.Set (Set)
import Data.Set qualified as Set

-- {len(two_let_codes)} two-letter codes
iso639_1 :: Set String
iso639_1 = Set.fromList {two_let_list}

-- {len(three_let_codes)} three-letter codes
iso639_2 :: Set String
iso639_2 = Set.fromList {three_let_list}"""
    return module


def to_haskell_list(py_set):

    item_strings = [f'"{item}"' for item in py_set]
    items = ",".join(item_strings)
    return "[" + items + "]"


def main():
    page = requests.get(URL)
    soup = BeautifulSoup(page.content, "html.parser")

    table = find_codes_table(soup)
    rows = table.find_all("tr")

    two_let_codes = set()
    three_let_codes = set()

    for row in rows:
        (two_let, three_let) = codes_from_row(row)
        for two in two_let:
            two_let_codes.add(two)
        for three in three_let:
            three_let_codes.add(three)

    print(haskell_module(two_let_codes, three_let_codes))


if __name__ == "__main__":
    main()
