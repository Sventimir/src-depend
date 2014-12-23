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


class DependTest(unittest.TestCase):

    def setUp(self):
        with FileMock('Test/Core.hs', ()) as file:
            self.moduleCore = HaskellModule(file)
        with FileMock('Test/IO.hs', ()) as file:
            self.moduleIO = HaskellModule(file)

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
        self.assertEqual(2, len(HaskellModule.registry))


if __name__ == '__main__':
    logging.basicConfig(level=logging.DEBUG, filename='test.log', filemode='w')
    unittest.main(exit=False)
    print('\nTestlog:')
    with open('test.log', 'r') as log:
        for line in log:
            print(line.strip())