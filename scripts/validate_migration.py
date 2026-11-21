#!/usr/bin/env python3
"""Augment a gay2scm report with real runtime loading and optional small renders."""
import argparse
import json
from pathlib import Path
import subprocess
import tempfile


def execute(command, timeout):
    result = subprocess.run(command, capture_output=True, text=True, timeout=timeout)
    if result.returncode:
        raise RuntimeError((result.stdout + result.stderr)[-2000:])
    return result.stdout


def main():
    cli = argparse.ArgumentParser(description=__doc__)
    cli.add_argument('report', type=Path)
    cli.add_argument('--probe', required=True, type=Path)
    cli.add_argument('--renderer', type=Path)
    cli.add_argument('--preview-dir', type=Path)
    cli.add_argument('--timeout', type=int, default=180)
    args = cli.parse_args()
    if bool(args.renderer) != bool(args.preview_dir):
        cli.error('--renderer and --preview-dir must be supplied together')
    rows = json.loads(args.report.read_text())
    failed = False
    for row in rows:
        if not row['converted']:
            failed = True
            continue
        row['loaded'] = False
        row['render_verified'] = False
        target = Path(row['output']).resolve()
        try:
            output = execute([str(args.probe.resolve()), str(target)], args.timeout)
            row['scene'] = json.loads(next(line[10:] for line in output.splitlines()
                                           if line.startswith('MIGRATION ')))
            row['loaded'] = True
            if args.renderer:
                args.preview_dir.mkdir(parents=True, exist_ok=True)
                image = args.preview_dir.resolve() / (target.stem + '-after.tga')
                with tempfile.NamedTemporaryFile(mode='w', suffix='.scm', dir=target.parent,
                                                 encoding='utf-8') as wrapper:
                    wrapper.write(target.read_text())
                    wrapper.write('\n(set-image-size (list 128 '
                                  f"{max(1, int(128 * row['scene']['height'] / row['scene']['width']))}))\n")
                    wrapper.flush()
                    execute([str(args.renderer.resolve()), '-b', '-j', '1',
                             # main.cpp expects paths relative to cwd.
                             str(Path(wrapper.name).relative_to(Path.cwd())),
                             str(image)], args.timeout)
                if not image.is_file() or image.stat().st_size < 18:
                    raise RuntimeError('renderer did not produce an image')
                row['render_verified'] = True
                row['preview'] = str(image)
            row.pop('validation_error', None)
        except (RuntimeError, OSError, ValueError, StopIteration, subprocess.TimeoutExpired) as error:
            row['validation_error'] = str(error)
            failed = True
    args.report.write_text(json.dumps(rows, indent=2) + '\n')
    return int(failed)

if __name__ == '__main__':
    raise SystemExit(main())
