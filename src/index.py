from pathlib import Path
import re
from sys import maxsize
import json


SOURCE_DIR = Path(__file__).parent
CODE_SOURCE_DIR = SOURCE_DIR / 'code/src'
FUNCTION_PATTERN = re.compile(r'^([^\s]*)?\s*::')
DATA_PATTERN = re.compile(r'^(?:data|newtype)\s+([A-Z][a-z0-9_\']*)')

EXCLUDE_FILES = [
  'FFT.hs'
]

EXCLUDE_FILES = [CODE_SOURCE_DIR / file for file in EXCLUDE_FILES]

index = {}

# Current indexing does not disambiguate between declaration in different 
# modules. In the document, templates are picked based on the functions that
# are cited in a test or challenge. If ambiguous declaration are removed and
# there is at least one unique declaration, the problem does not occur.
bad_funs = set()

for file in CODE_SOURCE_DIR.rglob('*'):
  if file.is_file() and file.name.endswith('.hs') and file not in EXCLUDE_FILES:
    print(file)
    with open(file, encoding='utf-8') as handle:
      file = str(file.relative_to(CODE_SOURCE_DIR))
      for i, line in zip(range(1, maxsize), handle.readlines()):
        if matches := FUNCTION_PATTERN.match(line) or DATA_PATTERN.match(line):
          fun = matches.group(1)
          if fun in index and index[fun]['file'] != file:
            print(
              f'Duplicate declaration of function or datatype "{fun}". '
              f'Previously defined at {index[fun]['file']}:{index[fun]['line']}. '
              f'Redefined at {file}:{i}.'
            )
            bad_funs.add(fun)
          else:
            index[fun] = {
              'file': file,
              'line': i
            }

for fun in bad_funs:
    del index[fun]

with open(SOURCE_DIR / 'index.json', 'w') as handle:
  json.dump(index, handle, indent='  ')

