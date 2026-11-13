#!/usr/bin/env python3
"""Strict pre-Scheme RayGay scene migration (Python 3, standard library only)."""
import argparse
import json
import os
from pathlib import Path
import re
import sys
import tempfile

TOKEN = re.compile(r'\s+|/\*.*?\*/|\#[^\n]*|"(?:\\.|[^"\\])*"|(?:\d+(?:\.\d*)?|\.\d+)(?:[eE][+-]?\d+)?|[$@][A-Za-z_][\w]*|[A-Za-z_][\w-]*|\+\+|--|\+=|-=|\*=|/=|==|!=|<=|>=|&&|\|\||.', re.S)
NUM = re.compile(r'(?:\d+(?:\.\d*)?|\.\d+)(?:[eE][+-]?\d+)?\Z')
OBJECTS = {'sphere', 'box', 'solidbox', 'cylinder', 'ellipsoid', 'torus', 'group',
           'object', 'mesh', 'extrusion', 'blob', 'superellipsoid', 'difference', 'intersection', 'union'}
SETTINGS = {'globalphotons', 'causticphotons', 'estimateradius', 'estimatesamples',
            'finalgatherrays', 'cachetolerance'}
CAMERA = {'position': 'pos', 'lookat': 'lookat', 'up': 'up', 'fov': 'fov', 'aa': 'aa'}

class ConversionError(ValueError):
    pass

def call(name, *args):
    return '(' + ' '.join([name, *args]) + ')'

def ordered(name, *args):
    """Scheme does not specify argument evaluation order; legacy nodes do."""
    if not args or all(re.fullmatch(r'[+-]?(?:[0-9.]+)(?:e[+-]?[0-9]+)?', arg, re.I)
                       or arg.startswith(('#(', "'", '"')) for arg in args):
        return call(name, *args)
    return '(let* (' + ' '.join(f'(arg{i} {v})' for i, v in enumerate(args)) + ') ' + call(name, *(f'arg{i}' for i in range(len(args)))) + ')'

def read_text(path):
    data = path.read_bytes()
    try:
        return data.decode('utf-8')
    except UnicodeDecodeError:
        return data.decode('latin-1')

class Parser:
    def __init__(self, source, source_path=None, output_path=None, chain=()):
        self.tokens = []
        line = 1
        for match in TOKEN.finditer(source):
            token = match.group()
            if not token.isspace() and not token.startswith(('#', '/*')):
                self.tokens.append((token, line))
            line += token.count('\n')
        self.index = 0
        self.source = source_path
        self.output = output_path.resolve() if output_path else None
        self.chain = chain
        self.variables = set()
        self.dependencies = []
        self.camera = {}
        self.width, self.aspect = '640', ('4', '3')

    def peek(self):
        return self.tokens[self.index][0] if self.index < len(self.tokens) else ''

    def error(self, message):
        line = self.tokens[min(self.index, len(self.tokens)-1)][1] if self.tokens else 1
        raise ConversionError(f'{self.source or "<input>"}:{line}: {message}')

    def take(self, expected=None):
        token = self.peek()
        if not token:
            self.error('unexpected end of input')
        if expected is not None and token != expected:
            self.error(f'expected {expected!r}, got {token!r}')
        self.index += 1
        return token

    def accept(self, token):
        if self.peek() == token:
            self.take()
            return True
        return False

    def variable(self, token):
        if not token.startswith(('$', '@')):
            self.error(f'expected variable, got {token!r}')
        # Distinguish scalar and vector namespaces, including names like $min.
        return ('legacy-s-' if token[0] == '$' else 'legacy-v-') + token[1:]

    def expr(self, minimum=31):
        token = self.take()
        if token in ('-', '+', '!'):
            value = self.expr(60)
            if token in ('+', '-') and re.fullmatch(r'[0-9.]+(?:e[+-]?[0-9]+)?', value, re.I):
                left = ('-' if token == '-' else '') + value
            else:
                left = call({'-': '-', '+': '+', '!': 'not'}[token], value)
        elif token == '(':
            left = self.expr(0)
            self.take(')')
        elif NUM.fullmatch(token):
            # Avoid octal-like spellings and retain floating division semantics.
            left = repr(float(token))
        elif token.startswith(('$', '@')):
            left = self.variable(token)
        elif token in ('PI', 'pi'):
            left = '3.141592653589793'
        elif token in ('random', 'sin', 'cos', 'abs', 'min', 'max', 'pow'):
            self.take('(')
            args = [self.expr()]
            while self.accept(','):
                args.append(self.expr())
            self.take(')')
            if token == 'random' and len(args) == 1:
                args.insert(0, '0.0')
            arity = 2 if token in ('random', 'min', 'max', 'pow') else 1
            if len(args) != arity:
                self.error(f'{token} expects {arity} arguments')
            left = ordered('expt' if token == 'pow' else token, *args)
        else:
            self.error(f'expected expression, got {token!r}')
        precedence = {'||': 10, '&&': 20, '==': 30, '!=': 30, '<': 30, '>': 30,
                      '<=': 30, '>=': 30, '+': 40, '-': 40, '*': 50, '/': 50}
        while self.peek() in precedence and precedence[self.peek()] >= minimum:
            op = self.take()
            right = self.expr(precedence[op] + 1)
            name = {'||': 'or', '&&': 'and', '==': '=', '!=': '='}.get(op, op)
            left = call(name, left, right) if op in ('||', '&&') else ordered(name, left, right)
            if op == '!=':
                left = call('not', left)
        return left

    def vector(self, size=3, flat=False):
        if self.peek().startswith('@'):
            return self.variable(self.take())
        if flat:
            return ordered('vector', *(self.expr(60) for _ in range(size)))
        self.take('<')
        depth = 0
        comma_separated = False
        for cursor in range(self.index, len(self.tokens)):
            token = self.tokens[cursor][0]
            if token == '>' and depth == 0:
                break
            if token == ',' and depth == 0:
                comma_separated = True
            if token == '(':
                depth += 1
            elif token == ')':
                depth -= 1
        values = []
        # '>' closes a vector, rather than being a comparison operator.
        for i in range(size):
            values.append(self.expr(31 if comma_separated else 60))
            if i != size-1:
                self.accept(',')
        self.take('>')
        if all(re.fullmatch(r'[+-]?[0-9.]+(?:e[+-]?[0-9]+)?', v, re.I) for v in values):
            return '#(' + ' '.join(values) + ')'
        return ordered('vector', *values)

    def material(self):
        if self.peek().startswith('$'):
            return self.variable(self.take())
        self.take('material')
        self.take('{')
        props = []
        while not self.accept('}'):
            key = self.take()
            if key in ('diffuse', 'specular'):
                value = self.texture() if self.peek() == 'texture' else self.vector()
            elif key in ('kd', 'ks', 'kt', 'eta', 'specpow'):
                value = self.expr()
            elif key == 'gloss':
                value = ordered('list', self.expr(), self.expr())
            else:
                self.error(f'unsupported material property {key!r}')
            props.extend(["'" + key, value])
        return ordered('make-material', ordered('list', *props))

    def texture(self):
        self.take('texture'); self.take('{')
        raw = self.take()
        if not raw.startswith('"'):
            self.error('expected texture filename')
        filename = json.loads(raw)
        if self.source:
            candidates = [self.source.parent / filename, Path('scenes') / filename]
            path = next((p.resolve() for p in candidates if p.is_file()), None)
            if path is None:
                self.error(f'missing asset {filename!r}')
            self.dependencies.append(str(path))
            filename = os.path.relpath(path, self.output.parent) if self.output else str(path)
        x, y = self.expr(), self.expr()
        interpolation = self.take()
        if interpolation not in ('none', 'bilinear', 'bicubic'):
            self.error('unknown texture interpolation')
        self.take('}')
        return ordered('make-texture', json.dumps(filename), x, y, "'" + interpolation)

    def path(self):
        if self.peek().startswith('$'):
            return self.variable(self.take())
        kind = self.take(); self.take('{')
        if kind == 'circle':
            args = [self.vector(), self.expr(), self.vector()]
        elif kind == 'linesegment':
            args = [self.vector(), self.vector()]
        elif kind == 'spiral':
            args = [self.path(), self.expr(), self.expr()]
            args.append('0.0' if self.peek() == '}' else self.expr())
        elif kind in ('bezierspline', 'catmullromspline'):
            vectors = []
            while self.peek() != '}':
                vectors.append(self.vector())
            args = [ordered('list', *vectors)]
        else:
            self.error(f'unsupported path {kind!r}')
        self.take('}')
        return ordered('make-catmullrom-spline' if kind == 'catmullromspline' else {'solidbox': 'make-solid-box', 'union': 'legacy-union'}.get(kind, 'make-' + kind), *args)

    def block(self):
        self.take('{')
        result = []
        while not self.accept('}'):
            result.append(self.statement())
        return call('begin', *result) if result else "'()"

    def obj(self):
        kind = self.take()
        if kind == 'object':
            obj = ordered('legacy-clone', self.variable(self.take()))
        elif kind == 'group':
            body = self.block()
            obj = call('legacy-group', call('lambda', '()', body))
        else:
            block = self.accept('{')
            mat = self.material() if (self.peek().startswith('$') and self.index + 1 < len(self.tokens) and self.tokens[self.index + 1][0] not in ('=', '++', '--', '+=', '-=')) or self.peek() == 'material' else None
            if kind in ('box', 'mesh', 'extrusion', 'blob', 'superellipsoid') and mat is None:
                self.error(f'{kind} requires a material')
            flat = not block
            if kind in ('sphere', 'cylinder'):
                radius = self.expr(60) if flat else self.expr()
                start = self.vector(flat=flat)
                args = [start]
                if kind == 'cylinder':
                    args.append(self.vector(flat=flat))
                args.append(radius)
            elif kind in ('box', 'solidbox', 'ellipsoid'):
                first, second = self.vector(flat=flat), self.vector(flat=flat)
                args = [second, first] if kind == 'ellipsoid' else [first, second]
            elif kind == 'torus':
                args = [self.expr(), self.expr()]
            elif kind in ('difference', 'intersection'):
                args = [self.obj(), self.obj()]
            elif kind == 'union':
                items = []
                while self.peek() != '}':
                    items.append(self.statement())
                args = [call('legacy-group', call('lambda', '()', call('begin', *items)))]
            elif kind == 'extrusion':
                args = [self.path(), self.expr(), self.expr(), self.expr()]
                kind = 'legacy-extrusion'
            elif kind == 'superellipsoid':
                args = [self.expr() for _ in range(4)]
            elif kind == 'blob':
                iso, weight, steps, accuracy = (self.expr() for _ in range(4))
                atoms = self.obj()
                args = [iso, weight, steps, accuracy, mat, ordered('legacy-flatten', atoms)]
                mat = None
                kind = 'legacy-blob'
            elif kind == 'mesh':
                if self.peek() == 'flat':
                    self.error('flat mesh has no equivalent in make-mesh')
                self.accept('phong')
                lists = []
                for section in ('vertices', 'triangles'):
                    self.take(section); self.take('{')
                    vectors = []
                    while self.peek() != '}':
                        vectors.append(self.vector())
                    self.take('}')
                    lists.append(ordered('list', *vectors))
                args = [mat, *lists]
                mat = None
            else:
                self.error(f'unsupported object {kind!r}')
            if mat:
                args.append(mat)
            elif kind in ('box', 'mesh', 'legacy-extrusion') and (not args or args[0] is None):
                self.error(f'{kind} requires a material')
            if block:
                self.take('}')
            obj = ordered(kind if kind.startswith('legacy-') else {'solidbox': 'make-solid-box', 'union': 'legacy-union'}.get(kind, 'make-' + kind), *args)
        transforms = []
        while self.peek() in ('rotate', 'translate', 'scale'):
            op = self.take(); braces = self.accept('{')
            flat = self.peek() not in ('<',) and not self.peek().startswith('@')
            if op == 'scale' and flat:
                args = [self.expr(60)]
            else:
                args = [self.vector(flat=flat)]
            if op == 'rotate':
                args.append(self.expr(60) if flat else self.expr())
            if braces:
                self.take('}')
            transforms.append((op, args))
        for op, args in transforms:
            obj = ordered(op, obj, *args)
        return obj

    def camera_form(self):
        props = [v for k, val in self.camera.items() for v in ("'" + k, val)]
        return ordered('set-camera', ordered('make-pinhole-camera', ordered('list', *props)))

    def image_form(self):
        a, b = self.aspect
        return call('set-image-size', call('list', call('inexact->exact', call('truncate', self.width)), call('inexact->exact', call('truncate', call('*', self.width, call('/', b, a))))))

    def statement(self):
        token = self.peek()
        if token in OBJECTS:
            return ordered('legacy-add', self.obj())
        token = self.take()
        if token.startswith(('$', '@')):
            name = self.variable(token)
            op = self.take()
            if op == '=':
                self.variables.add(name)
                if self.peek() in OBJECTS:
                    value = self.obj()
                elif self.peek() == 'material':
                    value = self.material()
                elif self.peek() in ('circle', 'spiral', 'linesegment', 'bezierspline', 'catmullromspline'):
                    value = self.path()
                elif token.startswith('@'):
                    value = self.vector()
                else:
                    value = self.expr()
            elif op in ('++', '--'):
                value = call('+' if op == '++' else '-', name, '1.0')
            elif op in ('+=', '-=', '*=', '/='):
                value = ordered(op[0], name, self.expr())
            else:
                self.error(f'unsupported assignment {op!r}')
            return call('set!', name, value)
        if token == 'renderer':
            renderer = self.take()
            if renderer not in ('raytracer', 'photonrenderer', 'pathtracer'):
                self.error(f'unknown renderer {renderer!r}')
            return call('set-renderer', json.dumps(renderer))
        if token == 'background':
            flat = self.accept('rgba')
            value = self.texture() if self.peek() == 'texture' else self.vector(4, flat)
            return ordered('set-background', value)
        if token == 'camera':
            self.take('{')
            while not self.accept('}'):
                key = self.take()
                if key not in CAMERA:
                    self.error(f'unsupported camera option {key!r}')
                self.camera[CAMERA[key]] = self.vector() if key in ('position', 'lookat', 'up') else self.expr()
            return self.camera_form()
        if token.startswith('camera-'):
            key = {'camera-position': 'pos', 'camera-up': 'up', 'camera-lookat': 'lookat', 'camera-field-of-view': 'fov', 'camera-aa': 'aa'}.get(token)
            if key is None:
                self.error(f'unsupported camera setting {token!r}')
            self.camera[key] = self.vector(flat=True) if key in ('pos', 'up', 'lookat') else self.expr(60)
            return self.camera_form()
        if token == 'image':
            self.take('{'); self.take('width'); self.width = self.expr()
            key = self.take()
            if key == 'aspect':
                self.aspect = (self.expr(), self.expr())
            elif key == 'height':
                self.aspect = (self.width, self.expr())
            else:
                self.error('expected image aspect or height')
            self.take('}')
            return self.image_form()
        if token in ('image-width', 'image-ratio'):
            if token == 'image-width':
                self.width = self.expr(60)
            else:
                self.aspect = (self.expr(60), self.expr(60))
            return self.image_form()
        if token in ('settings', 'photonmap'):
            block = self.accept('{')
            props = []
            while True:
                key = self.take().replace('-', '')
                if key not in SETTINGS:
                    self.error(f'unsupported renderer setting {key!r}')
                props.extend(["'" + key, self.expr() if block else self.expr(60)])
                if not block or self.accept('}'):
                    break
            return ordered('set-settings', ordered('list', *props))
        if token == 'light':
            block = self.accept('{'); kind = self.take()
            kind = 'sky' if kind == 'skylight' else kind
            args = []
            if kind == 'sky':
                args = [self.expr(), self.expr()]
            elif kind in ('point', 'area', 'spot'):
                args = [self.vector(flat=not block)]
                if kind != 'point':
                    args.append(self.vector(flat=not block))
                if block and kind != 'point':
                    args.extend(self.expr() for _ in range(3 if kind == 'area' else 2))
            else:
                self.error(f'unsupported light {kind!r}')
            if block:
                self.take('power'); args.append(self.vector()); self.take('}')
            elif self.peek() and (NUM.fullmatch(self.peek()) or self.peek() == '-'):
                args.append(self.vector(flat=True))
                if kind in ('area', 'spot'):
                    trailing = [self.expr(60) for _ in range(3 if kind == 'area' else 2)]
                    args = args[:-1] + trailing + args[-1:]
            name = {'point': 'make-pointlight', 'area': 'make-arealight', 'spot': 'make-spotlight', 'sky': 'make-skylight'}[kind]
            return ordered('legacy-add', ordered(name, *args))
        if token in ('repeat', 'while', 'if', 'do'):
            if token == 'do':
                body = self.block(); self.take('while'); self.take('(')
                condition = self.expr(0); self.take(')')
                return '(let loop () ' + body + ' (if ' + condition + ' (loop)))'
            self.take('('); condition = self.expr(0); self.take(')'); body = self.block()
            if token == 'if':
                result = call('if', condition, body, self.block() if self.accept('else') else "'()")
            elif token == 'while':
                result = '(let loop () (if ' + condition + ' (begin ' + body + ' (loop))))'
            else:
                # RepeatActionNode evaluates the count once and casts it to int.
                result = '(let ((count (legacy-repeat-count ' + condition + '))) (let loop ((i 0)) (if (< i count) (begin ' + body + ' (loop (+ i 1))))))'
            return result
        if token == 'include':
            name = self.take()
            while self.peek() in ('.', '/'):
                name += self.take() + self.take()
            name = name.strip('"')
            if not self.source:
                self.error('include needs a source filename')
            candidates = [self.source.parent / name, Path('scenes') / name]
            path = next((p.resolve() for p in candidates if p.is_file()), None)
            if path is None and name == 'materials.gay':
                path = Path(__file__).resolve().parent / 'legacy/materials.gay'
                # Recover only referenced definitions; never load unrelated missing textures.
                original = read_text(path)
                definitions = re.split(r'(?m)^material\s+', original)[1:]
                needed = {token for token, _ in self.tokens if token.startswith('$')}
                forms = []
                for definition in definitions:
                    parts = definition.splitlines()
                    header = parts[0].split()
                    if header[0] not in needed:
                        continue
                    if header[1] != 'plastic':
                        self.error(f'unsupported historical material {header[0]!r}')
                    props = []
                    for line in parts[1:]:
                        line = line.split('#', 1)[0].strip()
                        if not line:
                            continue
                        fields = line.split()
                        key, values = fields[0], fields[1:]
                        if key in ('diffuse', 'specular'):
                            props.extend(["'" + key, '#(' + ' '.join(values) + ')'])
                        elif key in ('kd', 'ks', 'kt', 'eta', 'specpow'):
                            props.extend(["'" + key, values[0]])
                        elif key == 'gloss':
                            props.extend(["'gloss", call('list', *values)])
                        else:
                            self.error(f'unsupported historical material property {key!r}')
                    variable = self.variable(header[0]); self.variables.add(variable)
                    forms.append(call('set!', variable, ordered('make-material', ordered('list', *props))))
                self.dependencies.append(str(path))
                return call('begin', *forms)
            if path is None:
                self.error(f'missing include {name!r}; restore its historical definition explicitly')
            if path in self.chain:
                self.error(f'cyclic include {path}')
            child = Parser(read_text(path), path, self.output, (*self.chain, path))
            forms = child.forms()
            self.variables.update(child.variables)
            self.dependencies.extend([str(path), *child.dependencies])
            return call('begin', *forms)
        reasons = {'3': '3ds import is absent from the current runtime (fragment objects.gay)',
                   'sor': 'legacy sor importer prepends zero points; exact migration needs an explicit compatibility policy',
                   'fog': 'fog is not exposed by the current Scheme scene API',
                   'filters': 'the current scene API cannot configure the legacy postprocessing stack'}
        self.error(reasons.get(token, f'unsupported construct {token!r}; no output written'))

    def forms(self):
        result = []
        while self.peek():
            result.append(self.statement())
        return result

    def convert(self):
        forms = self.forms()
        referenced = set(re.findall(r'legacy-[sv]-[A-Za-z_]\w*', ' '.join(forms)))
        undefined = referenced - self.variables
        if undefined:
            self.error('undefined variables: ' + ', '.join(sorted(undefined)))
        declarations = [call('define', name, '#f') for name in sorted(self.variables)]
        helpers = '; Generated by scripts/gay2scm.py. Original files remain authoritative.\n(load "lib/legacy-migration.scm")\n'
        if self.output:
            library = Path(__file__).resolve().parents[1] / 'scenes/lib/legacy-migration.scm'
            helpers = helpers.replace('\"lib/legacy-migration.scm\"', json.dumps(os.path.relpath(library, self.output.parent)))
        return helpers + '\n\n'.join([*declarations, *forms]) + '\n'

def main(argv=None):
    cli = argparse.ArgumentParser(description=__doc__)
    cli.add_argument('inputs', nargs='+', type=Path)
    cli.add_argument('--output-dir', required=True, type=Path)
    cli.add_argument('--check', action='store_true')
    cli.add_argument('--report', type=Path)
    args = cli.parse_args(argv)
    results, seen = [], set()
    for source in args.inputs:
        files = sorted(source.rglob('*.gay')) if source.is_dir() else [source]
        for path in files:
            if path.resolve() in seen:
                continue
            seen.add(path.resolve())
            relative = path.relative_to(source) if source.is_dir() else Path(path.name)
            target = args.output_dir / relative.with_suffix('.scm')
            row = dict(source=str(path), output=str(target), converted=False,
                       loaded=False, semantic_verified=False, render_verified=False)
            try:
                parser = Parser(read_text(path), path.resolve(), target.resolve(), (path.resolve(),))
                converted = parser.convert()
                if not args.check:
                    target.parent.mkdir(parents=True, exist_ok=True)
                    temporary = None
                    try:
                        with tempfile.NamedTemporaryFile(mode='w', encoding='utf-8',
                                                         dir=target.parent, delete=False) as stream:
                            temporary = Path(stream.name)
                            stream.write(converted)
                        # Atomic publication without replacing an existing destination.
                        os.link(temporary, target)
                    finally:
                        if temporary is not None:
                            temporary.unlink(missing_ok=True)
                row['converted'] = True
                row['dependencies'] = [os.path.relpath(dependency, Path.cwd())
                                       for dependency in parser.dependencies]
            except (ConversionError, OSError, ValueError) as error:
                row['error'] = str(error).replace(str(Path.cwd()) + '/', '')
            results.append(row)
    report = json.dumps(results, indent=2) + '\n'
    if args.report:
        args.report.write_text(report, encoding='utf-8')
    else:
        print(report, end='')
    return int(any(not row['converted'] for row in results))

if __name__ == '__main__':
    sys.exit(main())
