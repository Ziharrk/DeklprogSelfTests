from pathlib import Path
import re
from sys import maxsize
import json

# TODO add Haskell's data and newtype

SOURCE_DIR = Path(__file__).parent
CODE_SOURCE_DIR = SOURCE_DIR / 'code/src'
SIGNATURE_PATTERN = re.compile(r'^([^\s]*)?\s*::')

EXCLUDE_FILES = [
  'FFT.hs'
]

EXCLUDE_FILES = [CODE_SOURCE_DIR / file for file in EXCLUDE_FILES]

index = {}

for file in CODE_SOURCE_DIR.iterdir():
  if file.is_file() and file.name.endswith('.hs') and file not in EXCLUDE_FILES:
    with open(file, encoding='utf-8') as handle:
      file = str(file.relative_to(CODE_SOURCE_DIR))
      for i, line in zip(range(1, maxsize), handle.readlines()):
        if matches := SIGNATURE_PATTERN.match(line):
          fun = matches.group(1)
          if fun in index:
            print(
              f"Duplicate definition of function '{fun}'. "
              f"Previously defined at {index[fun]['file']}:{index[fun]['line']}. "
              f"Redefined at {file}:{i}."
            )
          else:
            index[fun] = {
              'file': file,
              'line': i
            }
                    
with open(SOURCE_DIR / 'index.json', 'w') as handle:
  json.dump(index, handle, indent='  ')

