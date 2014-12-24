import module
import logging
import re


class HaskellModule(module.Module):

    filename_ext = '.hs'
    registry = set()
    module_regex = re.compile(r'^module\s*([A-Za-z0-9.]+).*$')
    import_regex = re.compile(r'^import(\s*qualified)?\s*([A-Z][A-Za-z0-9.]*).*$')

    def _parse_file(self, file):
        self.__name = None
        for line in file:
            match = self.module_regex.match(line)
            if not match is None:
                self.__name = match.group(1)
                continue
            match = self.import_regex.match(line)
            if not match is None:
                logging.info('Found module {} in {}'.format(match.group(2), self.name))
                self._Module__dependencies[match.group(2)] = None
        if self.__name is None:
            self.__name = 'Main\n({})'.format(self.filename)

    @property
    def name(self):
        return self.__name

Module = HaskellModule
