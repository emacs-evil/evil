import re
from os import path
import json

from docutils import nodes
from docutils.parsers.rst import Directive

from sphinx import addnodes
from sphinx.domains import Domain, ObjType, Index
from sphinx.domains.std import StandardDomain
from sphinx.directives import ObjectDescription
from sphinx.roles import XRefRole
from sphinx.util.docfields import Field
from sphinx.util.nodes import make_refnode


with open(path.join(path.dirname(__file__), '..', '..', 'docstringdb.json')) as f:
    DATA = json.load(f)


re_evilcode = re.compile(r"`(evil-[^']*)'")
re_code = re.compile(r"`([^:][^ ']*)'")
re_kwd = re.compile(r"`(:[^']*)'")
re_item = re.compile(r"([^\n])\n- ")
re_sexp = re.compile(r"\([A-Z \-\.'`\[\]]+\)|[A-Z\-]+")
re_capitals = re.compile(r"[A-Z\-]+")
re_nonspace = re.compile(r"[^ ]")
re_signature = re.compile(r'\(fn (.*)\)')
re_keymap_or_kbd = re.compile(r"\\[\[<]([^\]>]*)[\]>]")

emphasis = [
    'thing-at-point',
]

def emacs_is_local(var):
    return DATA[var]['local']

def emacs_default_value(var):
    default = DATA[var]['default']
    tp = DATA[var]['default-type']
    if tp == 'string':
        rep = repr(default)[1:-1]
        return f'"{rep}"'
    return str(default)

def process_docstring(docstring, capitals=None):
    # Remove explicit signature
    docstring = re_signature.sub('', docstring)

    # Add code blocks to indented sections
    def blockified_lines(lines):
        in_block = False
        for line in lines:
            try:
                indented = next(re_nonspace.finditer(line)).start(0) > 3
            except StopIteration:
                indented = None
            if indented is True and not in_block:
                yield '.. code-block:: elisp'
                yield ''
                in_block = True
            elif indented is False:
                in_block = False
            yield line
    docstring = '\n'.join(blockified_lines(docstring.split('\n')))

    # Substitute `evil-alpha' with :elisp:ref:`evil-alpha`
    docstring = re_evilcode.sub(r':elisp:ref:`\1`', docstring)

    # Substitute `alpha' with ``alpha``
    docstring = re_code.sub(r'``\1``', docstring)

    # Substitute `:alpha' with ``alpha``
    docstring = re_kwd.sub(r'``\1``', docstring)

    # Translate key bindings
    keymap = None
    def substitute_binding(match):
        nonlocal keymap
        if match.group(0)[1] == '<':
            keymap = match.group(1)
            return ''
        if keymap is None:
            print(docstring)
            assert False
            return '???'
        key = DATA[keymap]['keymap-inv'][match.group(1)]
        return f':kbd:`{key}`'
    docstring = re_keymap_or_kbd.sub(substitute_binding, docstring)

    # Add empty line between list items
    docstring = re_item.sub(r'\1\n\n- ', docstring)

    if capitals is None:
        capitals = []
    else:
        capitals = list(capitals)

    # Find things that look like sexps
    def substitute_sexp(match):
        s = match.group(0)
        if re_capitals.match(s):
            if s in capitals:
                return f'*{s}*'
            return s
        else:
            capitals.extend(re_capitals.findall(s))
            return f'``{s}``'
    docstring = re_sexp.sub(substitute_sexp, docstring)

    # Italicize some words
    for s in emphasis:
        docstring = docstring.replace(s, f'*{s}*')

    return docstring

def emacs_variable_docstring(var):
    docstring = DATA[var]['var-docstring']
    return process_docstring(docstring)

def emacs_function_docstring(var):
    docstring = DATA[var]['fn-docstring']
    return process_docstring(docstring, capitals=emacs_argnames(var))

def emacs_argnames(var):
    arglist = emacs_arglist(var)
    return re_capitals.findall(arglist)

def emacs_arglist(var):
    docstring = DATA[var]['fn-docstring']
    match = re_signature.search(docstring)
    if match:
        return match.group(1)

    arglist = [arg.upper() for arg in DATA[var]['arglist']]
    state = None
    ret = ''
    for arg in arglist:
        if arg in ('&REST', '&OPTIONAL'):
            if state == '&OPTIONAL':
                ret += ']'
            state = arg
            ret += ' ['
            continue
        ret += ('' if state in ('&REST', '&OPTIONAL') else ' ') + arg
        if state == '&OPTIONAL':
            state += '-CONT'
    if state is not None and state.startswith('&OPTIONAL'):
        ret += ']'
    if state == '&REST':
        ret += '...]'
    return ret


class AbstractElisp(ObjectDescription):

    def add_target_and_index(self, name, sig, signode):
        anchor = f'elispobj-{sig}'
        signode['ids'].append(anchor)

        objs = self.env.domaindata['elisp']['objects']
        objs[sig] = {
            'docname': self.env.docname,
            'anchor': f'elispobj-{sig}',
            'type': self.object_type,
        }


class AbstractVariable(AbstractElisp):
    object_type = 'variable'

    def handle_signature(self, sig, signode):
        signode += addnodes.desc_annotation(sig, sig)
        return sig

    def run(self):
        extra = []

        default = self.default_value()
        if default:
            extra.append(f'Default: ``{default}``')
        if self.is_buffer_local():
            extra.append('buffer-local')

        self.content.data.extend(['', ', '.join(extra)])
        retval = super().run()
        return retval


class Variable(AbstractVariable):
    required_arguments = 1
    optional_arguments = 2

    def default_value(self):
        try:
            return self.arguments[1]
        except IndexError:
            return None

    def is_buffer_local(self):
        return 'bufloc' in self.arguments[1:]


class AutoVariable(AbstractVariable):
    required_arguments = 1

    def is_buffer_local(self):
        return emacs_is_local(self.arguments[0])

    def default_value(self):
        return emacs_default_value(self.arguments[0])

    def run(self):
        docstring = emacs_variable_docstring(self.arguments[0])
        self.content.data.extend(docstring.split('\n'))
        return super().run()


class AutoFunction(AbstractElisp):
    required_arguments = 1

    @property
    def object_type(self):
        return 'macro' if DATA[self.arguments[0]]['macrop'] else 'function'

    def handle_signature(self, sig, signode):
        args = emacs_arglist(sig)
        signode += addnodes.desc_annotation(sig, f'({sig} {args})')
        return sig

    def run(self):
        docstring = emacs_function_docstring(self.arguments[0])
        self.content.data.extend(docstring.split('\n'))
        return super().run()


class ElispIndex(Index):
    name = 'index'
    localname = 'Emacs lisp functions and variables'
    shortname = 'Elisp'

    def generate(self, docnames=None):
        index = {}
        for name, item in self.domain.data['objects'].items():
            if name.startswith('evil-'):
                letter = name[5].upper()
            else:
                letter = name[0].upper()
            index.setdefault(letter, []).append((
                name,
                0,
                item['docname'],
                item['anchor'],
                item['type'],
                '',
                '',
            ))

        index = {k: sorted(v, key=lambda k: k[0].lower()) for k, v in index.items()}
        index = list(index.items())
        index = sorted(index, key=lambda k: k[0])
        return index, True


class Elisp(Domain):
    name = 'elisp'
    label = 'Emacs lisp'

    object_types = {
        'variable': ObjType('variable', 'variable', 'obj'),
        'autovariable': ObjType('autovariable', 'autovariable', 'obj'),
        'autofunction': ObjType('autofunction', 'autofunction', 'obj'),
    }

    directives = {
        'variable': Variable,
        'autovariable': AutoVariable,
        'autofunction': AutoFunction,
    }

    roles = {
        'ref': XRefRole(),
    }

    initial_data = {
        'objects': {},
    }

    indices = {
        ElispIndex,
    }

    def resolve_xref(self, env, fromdocname, builder, typ, target, node, contnode):
        obj = self.data['objects'].get(target, None)
        if obj is None:
            return None
        return make_refnode(builder, fromdocname, obj['docname'], obj['anchor'], contnode, obj['anchor'])


def setup(app):
    app.add_domain(Elisp)
    StandardDomain.initial_data['labels']['elispindex'] = ('elisp-index', '', 'Emacs lisp functions and variables')
    StandardDomain.initial_data['anonlabels']['elispindex'] = ('elisp-index', '')

    return {
        'version': '0.1',
        'parallel_read_safe': True,
        'parallel_write_safe': True,
    }
