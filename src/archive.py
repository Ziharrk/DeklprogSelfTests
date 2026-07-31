import tempfile
from pathlib import Path
import os
import shutil
import subprocess

def template_or_solution(file, is_template):
  file = Path(file)

  flag = '-DTEMPLATE' if is_template else '-UTEMPLATE'

  with tempfile.NamedTemporaryFile(mode="w", delete=False) as tmp:
    subprocess.run(
      ['cpp', '-P', '-traditional-cpp', flag, str(file)],
      stdout=tmp,
      check=True,
    )
    tmp_name = tmp.name

  with open(tmp_name) as f:
    lines = f.readlines()

  while lines and not lines[0].strip():
    lines.pop(0)

  with open(file, "w") as f:
    f.writelines(lines)

  os.remove(tmp_name)


def make_archive(func, archive_name):
  items = [
    'src',
    'code.cabal'
  ]

  with tempfile.TemporaryDirectory() as project_dir:
    project_dir = Path(project_dir)

    for item in items:
      item = Path(__file__).parent / 'code' / item
      target = project_dir / item.name

      if item.is_dir():
        shutil.copytree(item, target, dirs_exist_ok=True)
      else:
        shutil.copy2(item, target)

    for source_file in project_dir.glob('**/*.hs'):
      func(source_file)

    shutil.make_archive(archive_name, 'zip', project_dir)

make_archive(lambda f: template_or_solution(f, True), 'templates')
make_archive(lambda f: template_or_solution(f, False), 'solutions')

