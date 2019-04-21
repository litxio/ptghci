
import pytest
from mock import *
from ptghci.completer import make_completer
from ptghci.settings import Settings

@pytest.fixture
def engine(scope='session'):

    from ptghci.engine import Engine
    settings = Settings.default_settings()
    # settings.verbosity = "Trace"
    # settings = Settings.from_settings_file("ptghci.yaml")
    ngin = Engine.oop_engine(settings)
    yield ngin
    ngin.finish()


@pytest.fixture
def completer(engine):
    return make_completer(engine, Settings.default_settings())
