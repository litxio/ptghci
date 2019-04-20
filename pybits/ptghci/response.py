
class Response:

    Value = "Value"
    Stream = "Stream"
    Error = "Error"

    def __init__(self, kind: str, content: str=None):
        self.kind = kind
        self.content = content

    @property
    def success(self):
        return self.kind != self.Error

    @classmethod
    def from_value(cls, val: str):
        return cls(cls.Value, val)

    @classmethod
    def from_error_message(cls, err_msg: str):
        return cls(cls.Error, err_msg)

    def __repr__(self):
        return "<%s Response: %s>" % (self.kind, repr(self.content))
