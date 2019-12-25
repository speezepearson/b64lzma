import argparse
from pathlib import Path
import bs4

parser = argparse.ArgumentParser()
parser.add_argument('target', type=Path, help='html file to inline resources into')

def main(target: Path):
    soup = bs4.BeautifulSoup(target.open(), "html.parser")

    for tag in soup.find_all('script'):
        if 'src' in tag.attrs:
            tag.string = (target.parent / tag.attrs['src']).read_text()
            del tag.attrs['src']

    target.write_text(str(soup))

if __name__ == '__main__':
    args = parser.parse_args()
    main(target=args.target)
