#!/usr/bin/env python3
"""Build a headless historical renderer for visual review, outside the worktree."""
import argparse
from pathlib import Path
import subprocess

REVISION = 'd6ec6b2baa5388fab032b8c2267184978c2f17ad'
ROOT = Path(__file__).resolve().parents[2]

def run(*args, cwd):
    subprocess.run(args, cwd=cwd, check=True)

def replace(root, filename, before, after):
    path = root / filename
    text = path.read_text()
    if before not in text:
        raise RuntimeError(f'reference patch no longer matches {filename}')
    path.write_text(text.replace(before, after))

def main():
    cli = argparse.ArgumentParser(description=__doc__)
    cli.add_argument('directory', type=Path, help='new, empty build directory')
    args = cli.parse_args()
    dest = args.directory.resolve()
    dest.mkdir(parents=True, exist_ok=False)
    archive = dest / 'source.tar'
    with archive.open('wb') as output:
        subprocess.run(['git', 'archive', REVISION], cwd=ROOT, stdout=output, check=True)
    run('tar', '-xf', str(archive), '-C', str(dest), cwd=ROOT)
    archive.unlink()
    patches = [
        ('src/stats.h', 'static Stats* Stats::uniqueInstance;', 'static Stats* uniqueInstance;'),
        ('src/photon/genericphotonmap.cpp', '::PhotonMap<PhotonType>(', '::PhotonMap('),
        ('src/objects/heightfield.h', 'HeightField::HeightField(', 'HeightField('),
        ('src/parser/assignments.h', 'Assignments::getUniqueInstance()', 'getUniqueInstance()'),
        ('src/main.cpp', 'result = new PreviewWindowGTK(w, h, abortRenderingCB);', 'result = NULL; // Headless reference build.'),
        ('Makefile.am', 'SUBDIRS = src test', 'SUBDIRS = src'),
        ('src/Makefile.am', 'bin_PROGRAMS = tracer bench', 'bin_PROGRAMS = tracer'),
        ('src/parser/Makefile.am', 'BUILT_SOURCES = parser.h', 'BUILT_SOURCES = parser.h\nparser.h: parser.hh\n\tcp parser.hh parser.h'),
    ]
    for filename, before, after in patches:
        replace(dest, filename, before, after)
    path = dest / 'src/types.h'
    text = path.read_text()
    for name, definition in [('uint', 'unsigned int'), ('ushort', 'unsigned short'),
                             ('uchar', 'unsigned char'), ('ulong', 'unsigned long')]:
        text = text.replace(f'#define {name} {definition}', f'typedef {definition} {name};')
    path.write_text(text)
    for filename, header in [('src/ray.h', 'cstddef'), ('src/objects/triangle.h', 'types.h'),
                             ('src/parser/parser.yy', 'unistd.h'), ('src/parser/parserclass.cpp', 'unistd.h')]:
        path = dest / filename
        include = f'#include "{header}"\n' if header == 'types.h' else f'#include <{header}>\n'
        if filename.endswith('.yy'):
            path.write_text(path.read_text().replace('%{', '%{\n' + include, 1))
        else:
            path.write_text(include + path.read_text())
    run('autoreconf', '-fi', cwd=dest)
    run('./configure', cwd=dest)
    run('make', '-j4', cwd=dest)
    print(dest / 'src/tracer')

if __name__ == '__main__':
    main()
