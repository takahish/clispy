from unittest import TestCase
from lispy.env import Env, standard_env

class UnitTestCase(TestCase):
    def test_standard_env(self):
        self.assertIsInstance(standard_env(), Env)
