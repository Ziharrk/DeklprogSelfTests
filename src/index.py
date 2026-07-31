from collections import defaultdict
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

index = defaultdict(list)

for file in CODE_SOURCE_DIR.rglob('*'):
  if file.is_file() and file.name.endswith('.hs') and file not in EXCLUDE_FILES:
    with open(file, encoding='utf-8') as handle:
      file = str(file.relative_to(CODE_SOURCE_DIR))
      seen = set()
      for i, line in zip(range(1, maxsize), handle.readlines()):
        if matches := FUNCTION_PATTERN.match(line) or DATA_PATTERN.match(line):
          fun = matches.group(1)
          if fun not in seen:  # in template or solution
            index[fun].append({
              'file': file,
              'line': i
            })
            seen.add(fun)

with open(SOURCE_DIR / 'index.json', 'w') as handle:
  json.dump(index, handle, indent='  ')

