import unittest
import logging
from module import Module
from plugins.haskell import HaskellModule


class FileMock:

    def __init__(self, filename, content):
        self.name = filename
        self.__content = content

    def __enter__(self):
        return self

    def __exit__(self, type, value, traceback):
        return True

    def __iter__(self):
        return iter(self.__content)


contentCore = (
        'module Test.Core where',
        '',
        'import Control.Monad (liftM)',
        'import System.Exit'
    )

contentIO = (
        'module Test.IO (',
        '    io',
        ') where',
        '',
        'import Test.Core'
    )

contentApp = (
        '{#- LANGUAGE GADTs -#}',
        'module Test.App where',
        'import qualified Test.Core as Core',
        'import qualified Test.IO as IO'
    )

class DependTest(unittest.TestCase):

    def createModule(self, name, descriptor):
        with descriptor as file:
            try:
                setattr(self, name, HaskellModule(file))
            except Exception as e:
                logging.error(e)
                raise e

    def assertOrderless(self, first, second):
        self.assertEqual(set(first), set(second))

    def setUp(self):
        self.createModule('moduleCore', FileMock('Test/Core.hs', contentCore))
        self.createModule('moduleIO', FileMock('Test/IO.hs', contentIO))
        self.createModule('moduleApp', FileMock('Test/App.hs', contentApp))

    def tearDown(self):
        del self.moduleCore
        del self.moduleIO
        HaskellModule.registry.clear()

    def test_name_splitting(self):
        self.assertEqual('Test/Core.hs', self.moduleCore.filename)
        self.assertEqual('Test', self.moduleCore.directory)
        self.assertEqual('Test.Core', self.moduleCore.name)

    def test_registry(self):
        self.assertTrue(self.moduleCore.registry is self.moduleIO.registry)
        self.assertTrue(self.moduleCore in HaskellModule.registry)
        self.assertTrue(self.moduleIO in HaskellModule.registry)

    def test_module_classes_have_different_registries(self):
        self.assertFalse(self.moduleCore in Module.registry)

    def test_module_is_not_gegistered_twice(self):
        with FileMock('Test/Core.hs', ()) as file:
            moduleCore = HaskellModule(file)
        self.assertEqual(3, len(HaskellModule.registry))

    def test_import_parsing(self):
        self.assertOrderless(
                ('Control.Monad', 'System.Exit'),
                self.moduleCore.raw_dependencies
            )
        self.assertOrderless(
                ('Test.Core', ),
                self.moduleIO.raw_dependencies
            )
        self.assertOrderless(
                ('Test.Core', 'Test.IO'),
                self.moduleApp.raw_dependencies
            )

    def test_module_search(self):
        HaskellModule.create_dependency_tree()
        self.assertOrderless(
                (None, None),
                set(self.moduleCore.dependencies)
            )
        self.assertOrderless(
                ('Test.Core', ),
                map(lambda el: el.name, self.moduleIO.dependencies)
            )
        self.assertOrderless(
                ('Test.Core', 'Test.IO'),
                map(lambda el: el.name, self.moduleApp.dependencies)
            )


if __name__ == '__main__':
    logging.basicConfig(
            level=logging.INFO,
            filename='test.log',
            filemode='w',
            format='[%(asctime)s; %(levelname)s]: %(message)s'
        )
    unittest.main(exit=False)
    print('\nTestlog:')
    with open('test.log', 'r') as log:
        for line in log:
            print(line.strip())
