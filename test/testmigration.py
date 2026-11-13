#!/usr/bin/env python3
"""Independent semantic checks plus the complete repository scene inventory."""
import argparse
import importlib.util
import json
from pathlib import Path
import subprocess
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[1]
spec = importlib.util.spec_from_file_location('gay2scm', ROOT / 'scripts/gay2scm.py')
migration = importlib.util.module_from_spec(spec)
spec.loader.exec_module(migration)
PROBE = None

class MigrationTests(unittest.TestCase):
    def parse(self, text):
        return migration.Parser(text).convert()

    def run_scene(self, source, assertions=''):
        if not PROBE:
            self.skipTest('pass --probe to run actual Scheme integration')
        with tempfile.TemporaryDirectory() as directory:
            target = Path(directory) / 'scene.scm'
            parser = migration.Parser(source, output_path=target)
            target.write_text(parser.convert() + '\n' + assertions)
            run = subprocess.run([PROBE, str(target)], text=True, capture_output=True, timeout=90)
            self.assertEqual(run.returncode, 0, run.stdout[-2000:] + run.stderr[-2000:])
            return json.loads(next(line[10:] for line in run.stdout.splitlines() if line.startswith('MIGRATION ')))

    def test_reject_unknown_without_output(self):
        with tempfile.TemporaryDirectory() as directory:
            source = Path(directory) / 'bad.gay'
            source.write_text('renderer raytracer\nunknown { 1 }')
            report = Path(directory) / 'report.json'
            target = Path(directory) / 'out'
            self.assertEqual(migration.main([str(source), '--output-dir', str(target), '--report', str(report)]), 1)
            self.assertFalse(target.exists())
            self.assertIn(':2:', json.loads(report.read_text())[0]['error'])

    def test_expressions(self):
        p = migration.Parser('2+3*4')
        p.expr()
        self.assertEqual(p.peek(), '')
        self.run_scene('$x = 2+3*4\n$y = 9.4/8',
                       '(if (not (= legacy-s-x 14)) (begin (display "FAIL: precedence") (/ 1 0)))\n'
                       '(if (> (abs (- legacy-s-y 1.175)) 0.000001) (begin (display "FAIL: division") (/ 1 0)))')

    def test_loops_clone_transform_and_hits(self):
        source = '''renderer raytracer
camera {position <1,2,30> lookat <0,0,0> up <0,1,0> fov 45 aa 0}
image {width 128 aspect 4 3}
$m = material {diffuse <0.2,0.3,0.4> kd 0.7 ks 0.2 specpow 12}
$prototype = group {sphere {$m 1 <2,0,0>}}
object $prototype rotate <0,0,1> 90 translate <10,0,0>
object $prototype
$i = 0
repeat(3) {$i++ sphere {$m 1 <$i,10,0>}}
do {$i += 2} while ($i < 7)
while ($i < 9) {$i++}
@v = <$i,2*3,4>
'''
        checks = '''
(define (near a b) (< (abs (- a b)) 0.000001))
(if (not (= legacy-s-i 9)) (begin (display "FAIL: loop bounds") (/ 1 0)))
(if (not (= (vector-ref legacy-v-v 1) 6)) (begin (display "FAIL: vector evaluation") (/ 1 0)))
; The stored prototype must still be at x=2 after transforming its clone.
(define hit (intersect (car legacy-s-prototype) #(2 0 5) #(0 0 -1)))
(if (or (not hit) (not (near (vector-ref (car hit) 2) 1))) (begin (display "FAIL: prototype mutated") (/ 1 0)))
(define moved (legacy-clone legacy-s-prototype))
(rotate moved #(0 0 1) 90)
(translate moved #(10 0 0))
(define hit2 (intersect (car moved) #(10 -2 5) #(0 0 -1)))
(if (or (not hit2) (not (near (vector-ref (car hit2) 2) 1))
        (not (near (vector-ref (cadr hit2) 2) 1))) (begin (display "FAIL: transform order/normal") (/ 1 0)))
'''
        result = self.run_scene(source, checks)
        self.assertEqual({key: result[key] for key in ('objects', 'lights', 'width', 'height', 'camera')},
                         dict(objects=5, lights=0, width=128, height=96, camera=[1,2,30]))
        for actual, expected in zip(result['bounds'][0], [9, -3, -1, 11, -1, 1]):
            self.assertTrue(abs(actual - expected) <= 1e-8 + 1e-6 * abs(expected))
        self.assertEqual(result['first_material'][:3], [0.7, 0.2, 12])

    def test_flat_and_block_syntax(self):
        result = self.run_scene('''renderer raytracer
image-ratio 4 3
image-width 128
camera-position 1 2 30
camera-up 0 1 0
camera-lookat 0 0 0
camera-field-of-view 45
camera-aa 0
$m = material {diffuse <1,1,1> kd 1 ks 0}
sphere $m 1 2 3 4
box {$m <-1 0 -1> <1 2 1>}
light point 1 2 30 1 1 1
''')
        self.assertEqual(result['objects'], 2)
        self.assertEqual(result['lights'], 1)
        self.assertEqual(result['width'], 128)

    def test_repeat_count_semantics(self):
        self.run_scene('$n = 3.9\n$i = 0\nrepeat($n) {$n=0 $i++}\n',
                       '(if (not (= legacy-s-i 3)) (/ 1 0))')
        if not PROBE:
            return
        with tempfile.TemporaryDirectory() as directory:
            target = Path(directory) / 'negative.scm'
            target.write_text(migration.Parser('repeat(-1) {$i=0}', output_path=target).convert())
            run = subprocess.run([PROBE, str(target)], capture_output=True, text=True, timeout=30)
            self.assertNotEqual(run.returncode, 0)
            self.assertIn('negative', run.stderr.lower())

    def test_random_and_vector_namespace(self):
        self.run_scene('$x = random(1)\n@x = <$x,random(2,3),0>\n',
                       '(if (or (< legacy-s-x 0) (> legacy-s-x 1)) (/ 1 0))\n'
                       '(if (or (< (vector-ref legacy-v-x 1) 2) '
                       '(> (vector-ref legacy-v-x 1) 3)) (/ 1 0))')

    def test_cycle_check_and_no_overwrite(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            source = root / 'a.gay'
            source.write_text('include "a.gay"')
            with self.assertRaisesRegex(migration.ConversionError, 'cyclic include'):
                migration.Parser(source.read_text(), source, root / 'a.scm', (source,)).convert()
            source.write_text('renderer raytracer')
            target = root / 'output'
            report = root / 'report.json'
            args = [str(source), '--output-dir', str(target), '--report', str(report)]
            self.assertEqual(migration.main([*args, '--check']), 0)
            self.assertFalse(target.exists())
            self.assertEqual(migration.main(args), 0)
            original = (target / 'a.scm').read_bytes()
            self.assertEqual(migration.main(args), 1)
            self.assertEqual((target / 'a.scm').read_bytes(), original)

    def test_repository_inventory(self):
        files = sorted((ROOT / 'scenes').rglob('*.gay'))
        self.assertEqual(len(files), 20)
        supported = {'superellipsoids', 'blobpyramid', 'blinds', 'balls-cube', 'city', 'egg-chair', 'elastic-ball', 'glass',
                     'greek-temple', 'photontest', 'staircase', 'torii', 'wiggle'}
        for path in files:
            with self.subTest(scene=path.stem), tempfile.TemporaryDirectory() as directory:
                target = Path(directory) / 'scene.scm'
                parser = migration.Parser(migration.read_text(path), path, target)
                if path.stem not in supported:
                    with self.assertRaises(migration.ConversionError):
                        parser.convert()
                    continue
                target.write_text(parser.convert())
                if PROBE:
                    run = subprocess.run([PROBE, str(target)], capture_output=True, text=True, timeout=90)
                    self.assertEqual(run.returncode, 0, run.stdout[-1000:] + run.stderr[-1000:])
                    self.assertIn('MIGRATION ', run.stdout)

if __name__ == '__main__':
    cli = argparse.ArgumentParser()
    cli.add_argument('--probe')
    args, rest = cli.parse_known_args()
    PROBE = str(Path(args.probe).resolve()) if args.probe else None
    unittest.main(argv=[__file__, *rest])
