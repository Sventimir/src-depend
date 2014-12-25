import os.path
import logging
import re

class Module:
    '''Base class for language plugins. In order to build your own language
    plugin, create a source file in plugins/directory and define a Module class
    there that inherits this one. Minimal implementation should contain:
        * filename_ext property overwritten;
        * preferably registry should be ovewritten with empty set;
        * _parse_file() method which parses out imported modules from given file
          and puts them as keys in _Module__dependecies dict (with None values).
    In order to use the plugin, call depend.py with --lang parameter, specifying
    your plugin's filename (without '.py' part). See plugins/haskell.py for an
    example.'''

    filename_ext = ''
    registry = set()

    @classmethod
    def create_dependency_tree(cls):
        for module in cls.registry:
            module.find_dependencies()

    @classmethod
    def remove_redundant_dependencies(cls):
        '''Iterates over registered modules and for each of them removes all
        direct dependencies that also exist indirectly. For instance if A
        depends on B and C, and B also depends on C, then dependency of A on C
        is considered redundant.'''
        redundant = {}
        for module in cls.registry:
            redundant[module] = module.find_redundant_deps()
            logging.debug('{} depends on: {}'.format(module.name, redundant[module]))
        for mod, deps in redundant.items():
            for d in deps:
                logging.debug('Removing redundant dependency of {} on {}.'.format(
                        mod.name, d
                    ))
                del mod.__dependencies[d]


    def __init__(self, file, root_dir=os.path.curdir):
        '''Constructor. Takes a file descriptor as an argument and reads it
        eagerly so that the descriptor can be closed rigth away after creating
        the Module object.'''
        filename = os.path.relpath(file.name, root_dir)
        filename = re.sub(self.filename_ext + '$', '', filename)
        self.__filename_components = tuple(os.path.split(filename))
        self.__dependencies = {}
        self.__attributes = {}
        self._parse_file(file)
        self._assign_attributes()
        self.registry.add(self)
        logging.debug('Module {} registered successfully.'.format(file.name))

    @property
    def directory(self):
        return os.path.join(*self.__filename_components[:-1])

    @property
    def name(self):
        return '.'.join(self._Module__filename_components).strip('.')

    @property
    def filename(self):
        return os.path.join(*self.__filename_components) + self.filename_ext

    @property
    def raw_dependencies(self):
        return self.__dependencies.keys()

    @property
    def dependencies(self):
        return self.__dependencies.values()

    @property
    def attributes(self):
        return self.__attributes

    def find_dependencies(self):
        for module_name in self.__dependencies.keys():
            for module in self.registry:
                if module_name == module.name:
                    self.__dependencies[module_name] = module
                    break

    def find_redundant_deps(self):
        redundant = set()
        direct_deps = set(self.raw_dependencies)
        for dep in direct_deps:
            if self.__dependencies[dep] is None:
                continue
            indirect_deps = set(self.__dependencies[dep].raw_dependencies)
            for red in direct_deps.intersection(indirect_deps):
                redundant.add(red)
        return redundant


    def __repr__(self):
        return '<{0} {1}>'.format(self.__class__.__name__, self.name)

    def __eq__(self, other):
        return self.__class__ is other.__class__ and self.name == other.name

    def __ne__(self, other):
        return not (self.__class__ is other.__class__ and self.name == other.name)

    def __hash__(self):
        """Required when __eq__ is overridden."""
        return hash(self.filename)

    def _parse_file(self, file):
        """Implemented in derived classes, it should at least set
        self._dependencies field to hold names of all modules this one depends
        upon."""
        raise NotImplementedError()

    def _assign_attributes(self):
        '''May optionally be overridden in derived class.'''
        pass
