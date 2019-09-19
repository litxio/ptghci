import os
import re
from prompt_toolkit.application.current import get_app
import yaml

DEFAULTS = {
    "inputPrompt": "\x1b[32;1m\u03BB [{lineno}]\x1b[0m: ",
    "historyPath": "~/.ptghci_history",
    "style": "default",
    "outPrompt": "\x1b[31;1mOut [{lineno}]\x1b[0m: ",
    "verbosity": "Info",
    'typeBarEnabled': True,
    'typeBarStyle': 'noreverse bg:#222222',
    'typeBarTextStyle': '',
    'magicPrefix': '%',
    'viMode': False,
}

class Settings():
    @classmethod
    def load(cls):
        settings_file = find_settings_file()
        if settings_file:
            return cls.from_settings_file(settings_file)
        else:
            return cls.default_settings()

    @classmethod
    def default_settings(cls):
        res = cls()
        res.settings_path = None
        for (k,v) in DEFAULTS.items():
            res.__dict__[to_snake(k)] = v
        return res

    @classmethod
    def from_settings_file(cls, path):
        with open(path) as f:
            d = yaml.load(f, Loader=yaml.Loader)

        if d is None:  # Empty file?
            d = {}

        for (k,v) in DEFAULTS.items():
            if k not in d:
                d[k] = v

        res = cls()
        res.settings_path = path
        for (k,v) in d.items():
            res.__dict__[to_snake(k)] = v
        return res

def find_settings_file():
    priority = ['ptghci.yaml', '.ptghci.yaml',
                os.path.expanduser('~/.ptghci.yaml')]
    for path in priority:
        if os.path.isfile(path):
            return path

    return None

def to_snake(camel):
    """ camelCase to snake_case """
    s1 = re.sub('(.)([A-Z][a-z]+)', r'\1_\2', camel)
    return re.sub('([a-z0-9])([A-Z])', r'\1_\2', s1).lower()
