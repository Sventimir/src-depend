import module
import logging
import re

class Module(module.Module):

    filename_ext = '.py'
    registry = set()
    import_regexes = (
            re.compile(r'^\s*import\s+([A-Za-z0-9._]+).*$'),
            re.compile(r'^\s*from\s+([A-Za-z0-9._]+)\s+import.*$')
        )

    def _parse_file(self, file):
        for line in file:
            for regex in self.import_regexes:
                match = regex.match(line)
                if not match is None:
                    logging.debug('Found module {} in {}.'.format(match.group(1), self.name))
                    self._Module__dependencies[match.group(1)] = None
