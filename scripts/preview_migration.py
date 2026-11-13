#!/usr/bin/env python3
"""Render small historical/current pairs; Pillow is used only for lossless PNG encoding."""
import argparse
import hashlib
import json
from pathlib import Path
import re
import subprocess
import tempfile
from PIL import Image
from gay2scm import Parser, read_text

ROOT = Path(__file__).resolve().parents[1]
REVISION = 'd6ec6b2baa5388fab032b8c2267184978c2f17ad'


def main():
    cli = argparse.ArgumentParser(description=__doc__)
    cli.add_argument('--reference', required=True, type=Path)
    cli.add_argument('--renderer', required=True, type=Path)
    cli.add_argument('--output-dir', required=True, type=Path)
    cli.add_argument('--scenes', nargs='+', default=['egg-chair', 'staircase', 'elastic-ball', 'greek-temple'])
    args = cli.parse_args()
    args.output_dir.mkdir(parents=True, exist_ok=True)
    manifest = dict(reference_revision=REVISION, width=128, height=96,
                    aa=0, threads=1, seed=1, pairs=[])
    for name in args.scenes:
        source = ROOT / 'scenes/old' / (name + '.gay')
        text = re.sub(r'width\s+\d+', 'width 128', read_text(source))
        text = re.sub(r'\baa\s+\d+', 'aa 0', text)
        pair = dict(source=str(source.relative_to(ROOT)), images={})
        with tempfile.TemporaryDirectory() as directory:
            temp = Path(directory).resolve()
            old = temp / 'before.gay'
            # Bison's whitespace-vector production is ambiguous for negative
            # coordinates. Commas disambiguate literal vectors without changing values.
            old.write_text(re.sub(r'<([+\-.\deE\s]+)>',
                                  lambda m: '<' + ','.join(m.group(1).split()) + '>', text))
            new = temp / 'after.scm'
            new.write_text(Parser(text, source, new).convert())
            for side, exe, flag, scene in [('before', args.reference, '-x', old),
                                           ('after', args.renderer, '-b', new)]:
                output = temp / (side + '.tga')
                command = [str(exe.resolve()), flag, '-j', '1', scene.name, str(output)]
                result = subprocess.run(command, cwd=temp, capture_output=True, text=True, timeout=180)
                log = result.stdout + result.stderr
                (args.output_dir / f'{name}-{side}.log').write_text(log)
                if result.returncode or not output.is_file():
                    raise RuntimeError(f'{name}/{side}: {log[-2000:]}')
                target = args.output_dir / f'{name}-{side}.png'
                with Image.open(output) as image:
                    image.convert('RGB').save(target)
                pair['images'][side] = dict(file=target.name, sha256=hashlib.sha256(target.read_bytes()).hexdigest())
                counts = re.findall(r'Kd-tree(?:/Objects| objects added): (\d+)', log)
                if counts:
                    pair['images'][side]['kd_objects'] = int(counts[-1])
        manifest['pairs'].append(pair)
    (args.output_dir / 'manifest.json').write_text(json.dumps(manifest, indent=2) + '\n')

if __name__ == '__main__':
    main()
