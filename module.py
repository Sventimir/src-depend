import os.path
import logging
import re

class Module:

    filename_ext = ''
    registry = set()

    @classmethod
    def create_dependency_tree(cls):
        for module in cls.registry:
            module.find_dependencies()

    def __init__(self, file):
        '''Constructor. Takes a file descriptor as an argument and reads it
        eagerly so that the descriptor can be closed rigth away after creating
        the Module object.'''
        filename = re.sub(self.filename_ext + '$', '', file.name)
        self.__filename_components = tuple(os.path.split(filename))
        self.__dependencies = {}
        self._parse_file(file)
        self.registry.add(self)
        logging.debug('Module {} registered successfully.'.format(file.name))

    @property
    def directory(self):
        return os.path.join(*self.__filename_components[:-1])

    @property
    def name(self):
        return '.'.join(self._Module__filename_components)

    @property
    def filename(self):
        return os.path.join(*self.__filename_components) + self.filename_ext

    @property
    def raw_dependencies(self):
        return self.__dependencies.keys()

    @property
    def dependencies(self):
        return self.__dependencies.values()

    def find_dependencies(self):
        for module_name in self.__dependencies.keys():
            for module in self.registry:
                if module_name == module.name:
                    self.__dependencies[module_name] = module
                    break

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
