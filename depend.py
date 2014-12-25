#! /usr/bin/python
"""Src-depend is a simple tool for sketching source code dependency graphs
from source code itself. It iterates through all source code files in given
directory, finds import statements and turns them into edges of a dependency
graph.

Uses graphviz for sketching graphs."""
import argparse
import graphviz
import logging
import os.path
import re


def parseargs():
    parser = argparse.ArgumentParser()
    parser.add_argument('-l', '--lang', dest='lang', default='python',
        help='specifies language plugin to be used (defaults to python)')
    parser.add_argument('-o', '--output', dest='img_out',
        help='output sketched graph to specified file (appends extension automatiaclly)')
    parser.add_argument('-d', '--debug', dest='debug', action='store_true',
        help='debug mode')
    parser.add_argument('-f', '--output-format', dest='format', default='png',
        help='specifies output image\'s format (defaults to .png')
    parser.add_argument('-e', '--exclude', dest='excludes', nargs='+', default=[],
        help='a filename to ommit (multiple names possible)')
    parser.add_argument('--exclude-regex', dest='exclude-regex', default=None,
        help='filenames matching specified regex will be ignored')
    parser.add_argument('-q', '--quiet', dest='quiet', action='store_true',
        help='quiet mode')
    parser.add_argument('target', help='source code directory to analyse')
    return parser.parse_args().__dict__


def main(args):
    log_level = logging.INFO
    if args['debug']:
        log_level = logging.DEBUG
    elif args['quiet']:
        log_level = logging.ERROR
    logging.basicConfig(
            level=log_level,
            format='[%(asctime)s; %(levelname)s]: %(message)s'
        )

    is_excluded = exclude_checker(args['excludes'], args['exclude-regex'])

    try:
        import_obj = __import__('plugins.{}'.format(args['lang']))
        plugin = getattr(import_obj, args['lang'])
    except ImportError:
        logging.error('Could not find plugin for {}!'.format(args['lang']))
        return 1

    files = find_source_files(args['target'], plugin.Module.filename_ext, is_excluded)
    for f in files:
        with open(f, 'r') as file:
            plugin.Module(file, args['target'])

    plugin.Module.create_dependency_tree()
    graph = make_graph(*plugin.Module.registry)
    graph.format = args['format']

    if not args['img_out'] is None:
        output = graph.render(args['img_out'])
        logging.info('Writing graph image to {}...'.format(output))


def make_graph(*modules):
    graph = graphviz.Digraph()
    for module in modules:
        graph.node(module.filename, module.name)
        logging.debug('Creating node {}...'.format(module.name))
        for dep in module.dependencies:
            if not dep is None:
                logging.debug('Creating dependency of {} on {}'.format(
                        module.name, dep.name
                    ))
                graph.edge(module.filename, dep.filename)
    return graph


def find_source_files(path, ext, is_excluded):
    basename = os.path.basename(path)
    if is_excluded(basename):
        logging.debug('Ommitting excluded path: {}...'.format(path))
    elif not basename == '.' and basename.startswith('.'):
        logging.debug('Ommitting hidden path: {}...'.format(path))
    elif os.path.isfile(path) and path.endswith(ext):
        logging.info('{} recoginzed as source file.'.format(path))
        yield path
    elif os.path.isdir(path):
        logging.debug('In dir "{}": {}'.format(path, os.listdir(path)))
        for f in os.listdir(path):
            for el in find_source_files(os.path.join(path, f), ext, is_excluded):
                yield el
    else:
        logging.debug('{} is not a source file.'.format(path))


def exclude_checker(excluded, regex):
    if regex is None:
        return lambda filename: filename in excluded
    else:
        compiled_regex = re.compile(regex)
        return lambda filename:filename in excluded \
                or compiled_regex.match(filename)


if __name__ == '__main__':
    exit(main(parseargs()))
