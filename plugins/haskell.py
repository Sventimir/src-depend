import module
import logging
import re


class HaskellModule(module.Module):

    filename_ext = '.hs'
    registry = set()
    module_regex = re.compile(r'^module\s*([A-Za-z0-9._]+).*$')
    import_regex = re.compile(r'^import(\s*qualified)?\s*([A-Z][A-Za-z0-9._]*).*$')

    def _parse_file(self, file):
        self.__name = None
        for line in file:
            match = self.module_regex.match(line)
            if not match is None:
                self.__name = match.group(1)
                continue
            match = self.import_regex.match(line)
            if not match is None:
                logging.debug('Found module {} in {}'.format(match.group(2), self.name))
                self._Module__dependencies[match.group(2)] = None
        if self.__name is None:
            self.__name = 'Main\n({})'.format(self.filename)

    def _assign_attributes(self):
        package = ''
        if self.__name.startswith('Main'):
            package = 'main'
        elif '.' in self.__name:
            package = self.__name.rsplit('.', 1)
        if package != '':
            self._Module__attributes['group'] = package[0]

    @property
    def name(self):
        return self.__name

Module = HaskellModule
