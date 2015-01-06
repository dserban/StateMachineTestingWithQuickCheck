import flask
from collections import deque

app = flask.Flask(__name__)
circular_buffer = deque(maxlen=10)

@app.route('/pushright/<int:element>', methods=['GET','POST'])
def pushright(element):
    global circular_buffer

    circular_buffer.append(element)

    print circular_buffer
    return str(circular_buffer.__len__())

@app.route('/popleft', methods=['GET','POST'])
def popleft():
    global circular_buffer
    try:
        circular_buffer.popleft()
    except IndexError:
        pass
    print circular_buffer
    return str(circular_buffer.__len__())

@app.route('/cblength')
def cblength():
    global circular_buffer
    return str(circular_buffer.__len__())

@app.route('/clear', methods=['GET','POST'])
def clear():
    global circular_buffer
    circular_buffer.clear()
    print circular_buffer
    return str(circular_buffer.__len__())

if __name__ == '__main__':
    app.config['DEBUG'] = True
    app.run(host='0.0.0.0')

#    #
#    #
#    #
#    # First bug:
#    # When the new element to be inserted is divisible by 131
#    # Our program will refuse to perform the push-right operation
#    if element % 131 != 0:
#        circular_buffer.append(element)
#    #
#    #
#    #
#    # Second bug:
#    # The circular buffer is not empty
#    # AND the leftmost element is divisible by 17
#    # AND the new element to be inserted is divisible by 19
#    # Our program will refuse to perform the push-right operation
#    perform_append = True
#    try:
#        leftmost = circular_buffer.__getitem__(0)
#        if leftmost % 17 == 0 and element % 19 == 0:
#            perform_append = False
#    except:
#        pass
#    if perform_append:
#        circular_buffer.append(element)
#    #
#    #
#    #
