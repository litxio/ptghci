
"""
For testing the interface between ptghci frontend and the (Haskell) engine.
"""
import time
import threading

def test_execute(engine):
    res = engine.execute("1+1")
    assert res.success
    assert res.content.strip() == '2'
    
def test_type(engine):
    res = engine.get_type("(1 :: Integer)")
    assert res.success
    assert res.content == '(1 :: Integer) :: Integer'

def test_it(engine):
    """ check that the special 'that' variable works """
    engine.execute("1+1")
    res = engine.execute("that")
    assert res.success
    assert res.content.strip() == '2'


# TODO
# def test_interrupt(engine):
#     class RunThread(threading.Thread):
#         def __init__(self):
#             super().__init__()
#         def run(self):
#             res = engine.execute("sum [1..10000000000] :: Integer")
#             print(res)
#             assert res['content'] == "ABCD"
# 
#     rt = RunThread()
#     rt.start()
# 
#     time.sleep(.2)
# 
#     engine.send_interrupt()






