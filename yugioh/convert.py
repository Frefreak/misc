import re
import argparse
from cardsearch import extract_name_from_link, keyword_to_link_ja, duckduckgo_search_link

parser = argparse.ArgumentParser()
parser.add_argument('file')

if __name__ == '__main__':
    args = parser.parse_args()
    with open(args.file) as f:
        content = f.read().strip()
    for l in content.splitlines():
        match l.split('*'):
            case [n, num]:
                link = keyword_to_link_ja(n)
                if link is not None:
                    print(extract_name_from_link(link), 'x', num)
                else:
                    n = re.sub(r'\(.*?\)', '', n)
                    n = re.sub(r' ', '', n)
                    link = duckduckgo_search_link(n)
                    if link is not None:
                        print(extract_name_from_link(link), 'x', num)
                    else:
                        print(f'missing: \x1b[31;1m{n}\x1b[0m')

            case other:
                print('error formatting:', other)
