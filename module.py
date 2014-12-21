import os.path
import re

class Module:

    filename_ext = ''
    registry = set()

    def __init__(self, file):
        '''Constructor. Takes a file descriptor as an argument and reads it
        eagerly so that the descriptor can be closed rigth away after creating
        the Module object.'''
        filename = re.sub(self.filename_ext + '$', '', file.name)
        self.__filename_components = tuple(os.path.split(filename))
        self.registry.add(self)

    @property
    def directory(self):
        return os.path.join(*self.__filename_components[:-1])

    @property
    def name(self):
        return '.'.join(self._Module__filename_components)

    @property
    def filename(self):
        return os.path.join(*self.__filename_components) + self.filename_ext

    def __repr__(self):
        return '<{0} {1}>'.format(self.__class__.__name__, self.name)

    def __eq__(self, other):
        return self.__class__ is other.__class__ and self.name == other.name

    def __ne__(self, other):
        return not (self.__class__ is other.__class__ and self.name == other.name)

    def __hash__(self):
        """Required when __eq__ is overridden."""
        return hash(self.filename)
