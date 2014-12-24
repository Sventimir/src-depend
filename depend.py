"""Src-depend is a simple tool for sketching source code dependency graphs
from source code itself. It iterates through all source code files in given
directory, finds import statements and turns them into edges of a dependency
graph.

uses graphviz for sketching graphs."""
import argparse
import graphviz
import logging
import os.path


def parseargs():
    parser = argparse.ArgumentParser()
    parser.add_argument('-l', '--lang', dest='lang', default='python')
    parser.add_argument('-s', '--source', dest='src_out')
    parser.add_argument('-i', '--image', dest='img_out')
    parser.add_argument('-d', '--debug', dest='debug', action='store_true')
    parser.add_argument('-e', '--exclude', dest='exclude', nargs='+', default=[])
    parser.add_argument('-q', '--quiet', dest='quiet', action='store_true')
    parser.add_argument('target')
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
    try:
        import_obj = __import__('plugins.{}'.format(args['lang']))
        plugin = getattr(import_obj, args['lang'])
    except ImportError:
        logging.error('Could not find module {}!'.format(args['lang']))
        return 1
    for f in find_source_files(
            args['target'], plugin.Module.filename_ext, args['exclude']
        ):
        with open(f, 'r') as file:
            plugin.Module(file)
    plugin.Module.create_dependency_tree()
    graph = make_graph(*plugin.Module.registry)
    graph.format = 'png'
    write_file(args['src_out'], graph.source, 'source')

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


def find_source_files(path, ext, excludes):
    basename = os.path.basename(path)
    if basename in excludes:
        logging.debug('Ommitting excluded path: {}...'.format(path))
    elif not basename == '.' and basename.startswith('.'):
        logging.debug('Ommitting hidden path: {}...'.format(path))
    elif os.path.isfile(path) and path.endswith(ext):
        logging.info('{} recoginzed as source file.'.format(path))
        yield path
    elif os.path.isdir(path):
        logging.debug('In dir "{}": {}'.format(path, os.listdir(path)))
        for f in os.listdir(path):
            for el in find_source_files(os.path.join(path, f), ext, excludes):
                yield el
    else:
        logging.debug('{} is not a source file.'.format(path))


def write_file(file, content, what):
    if not file is None:
        try:
            with open(file, 'w') as file:
                logging.info('Writing graph {} to: {}...'.format(what, file.name))
                file.writelines(content)
        except IOError:
            logging.error('Could not write to: {}!'.format(file))


if __name__ == '__main__':
    exit(main(parseargs()))
