if !has('python')
    echo "Note: you need +python enabled to use malk interactive mode"
    finish
endif

function! MalkInit()
python << EOF
import vim
import json
import socket

global buffers
global doc_ids
buffers = {}
doc_ids = {}

class Buffer:
    def __init__(self, doc_id):
        self.doc_id = doc_id
        self.synced_changenr = 0

    def apply_diff(self, diff):
        (r0, c0) = pos_to_row_col(diff["start_pos"])
        (r1, c1) = pos_to_row_col(diff["start_pos"] + diff["remove_len"])
        #print("pos %d %d %d %d" % (r0, c0, r1, c1))
        buf = vim.current.buffer
        tail = buf[r1][c1:]
        del buf[(r0 + 1):(r1 + 1)]
        ins = diff["insert_text"].split("\n")
        buf[r0] = buf[r0][:c0] + ins[0]
        buf[(r0 + 1):(r0 + 1)] = ins[1:]
        buf[r0 + len(ins) - 1] += tail

def malk_init():
    global sock
    global fail_notified

    try:
        sock
        return True
    except NameError:
        try:
            print("reconnecting to malk daemon")
            sock = socket.socket()
            sock.connect(("0.0.0.0", 45666))
            sock = sock.makefile(mode="b")
            fail_notified = False
            return True
        except Exception as e:
            del sock
            try:
                assert fail_notified
                return False
            except Exception:
                fail_notified = True
                print("failed to connect to malkc daemon: %s" % str(e))
                return False

"""
def malk_process_msg(msg):
    if not "kind" in msg:
        return
    kind = msg["kind"]
    if kind == "loaded":
        doc_id = msg["doc_id"]
        return
    if kind == "apply_diff":
        pattern = ""
        for c in msg["old_match"]:
            if c == '\n':
                pattern += '\\n'
                continue
            if c == '\\' or c == '/':
                pattern += '\\'
            pattern += c

        text = ""
        for c in msg["new_text"]:
            if c == '/':
                text += '\\'
            text += c
        command = "%s/\V\C" + pattern + "/" + text + "/g"
        vim.command("echo 'command: " + command + "'")
        vim.command(command)
"""

def malk_call(request):
    if not malk_init():
        return
    request = json.dumps(request)
    request = request + "\n"

    sock.write(request.encode("utf-8"))
    sock.flush()
    # TODO: skipping empty lines should not be necessary
    line = "\n"
    while line == "\n":
        line = sock.readline()
    result = json.loads(line)

    if "result" in result:
        result = result["result"]
        return result
        #malk_process_msg(result)
    elif "error" in result:
        error = result["error"]["message"]
        print("error from daemon: %s" % str(error))
        

def malk_load():
    global buffers

    if not malk_init():
        return

    b = vim.current.buffer

    text = "\n".join(b[:])
    request = {
        "jsonrpc": "2.0",
        "method": "load_buffer",
        "params": {
            "stamp": 0,
            "text": text,
        },
        "id": 0,
    }
    result = malk_call(request)
    doc_id = result["doc_id"]
    doc_ids[doc_id] = b
    buffers[b] = Buffer(doc_id)

def file_length():
    num_lines = len(vim.current.buffer[:])
    return line_pos(num_lines - 1) + len(vim.current.buffer[-1])

def line_pos(line):
    return int(vim.eval("line2byte(%d)" % (line + 1))) - 1

def pos_to_row_col(pos):
    low = 0
    high = len(vim.current.buffer[:])
    while high - low >= 2:
        mid = (high + low) / 2
        b = line_pos(mid)
        if b <= pos:
            low = mid
        else:
            high = mid
    row = low
    col = pos - line_pos(row)
    return (row, col)

def current_doc_id():
    global buffers
    return buffers[vim.current.buffer].doc_id

def _undo_to(n):
    n = int(n)
    if n == 0:
        vim.command('silent earlier %s' % (int(vim.eval('&undolevels')) + 1))
    else:
        vim.command('silent undo %d' % n)


def malk_normalise():
    if not malk_init():
        return

    doc_id = current_doc_id()
    pos = vim.current.window.cursor
    request = {
        "jsonrpc": "2.0",
        "method": "normalise",
        "params": {
            "doc_id": doc_id,
            "pos": pos,
        },
        "id": 0,
    }
    result = malk_call(request)
    if result["kind"] == "apply_diff":
        buffers[vim.current.buffer].apply_diff(result)

def malk_edit():
    global buffers
    if not malk_init():
        return

    changenr = int(vim.eval("changenr()"))
    #print("at changenr %d and buffer is %s" % (changenr, vim.current.buffer[:]))
    #print("although entries == %s" % repr(vim.eval("undotree()")["entries"]))
    b = buffers[vim.current.buffer]
    synced_changenr = b.synced_changenr
    if changenr == synced_changenr:
        return
    after = vim.current.buffer[:]
    cursor_pos = vim.current.window.cursor
    #print("saving cursor pos: %s" % repr(cursor_pos))
    _undo_to(synced_changenr)
    before = vim.current.buffer[:]
    
    l = 0
    while l < len(before) and l < len(after) and before[l] == after[l]:
        l += 1

    if l == len(before):
        #print("adding lines at the end")
        start_pos = file_length()
        remove_len = 0
        insert_text = "\n" + "\n".join(after[l:])
        #print("synced buffer == %s" % repr(vim.current.buffer[:]))
        _undo_to(changenr)
        #print("restored to changenr %d and buffer is %s" % (changenr, vim.current.buffer[:]))
        #vim.current.buffer[:] = after
        #print("current buffer == %s" % repr(vim.current.buffer[:]))
    elif l == len(after):
        #print("removing lines at the end")
        fl = file_length()
        start_pos = line_pos(l) - 1
        remove_len = fl - start_pos
        #print("fl == %d, remove_len == %d" % (fl, remove_len))
        insert_text = ""
        _undo_to(changenr)
        #vim.current.buffer[:] = after
    else:
        c = 0
        while c < len(before[l]) and c < len(after[l]) and before[l][c] == after[l][c]:
            #print("c == %d" % c)
            #print("before[l] == %s" % repr(before[l]))
            #print("after[l] == %s" % repr(after[l]))
            c += 1
        
        #print("finally c == %d" % c)

        start_pos = line_pos(l) + c

        #print("%s" % repr(before))
        #print("%s" % repr(after))
        le = 1
        while len(before) - le > l and len(after) - le > l and before[-le] == after[-le]:
            #print("the same! le == %d" % le)
            #print("before %s" % repr(before[-le]))
            #print("after %s" % repr(after[-le]))
            le += 1
        #print("le == %d" % le)

        ce = 1
        while ce <= len(before[-le]) and ce <= len(after[-le]) and (len(before) - le != l or len(before[l]) - ce >= c) and (len(after) - le != l or len(after[l]) - ce >= c) and before[-le][-ce] == after[-le][-ce]:
            #print("the same! ce == %d" % ce)
            #print("before %s" % repr(before[-le]))
            #print("after %s" % repr(after[-le]))
            ce += 1
            #print("%s %s %s %s %s" % (ce <= len(before[-le]), ce <= len(after[-le]), (len(before) - le != l or len(before[l]) - ce >= c), (len(after) - le != l or len(after[l]) - ce >= c), before[-le][-ce] == after[-le][-ce]))
        ce -= 1

        #print("ce == %d" % ce)
        #print("some things: %d %d %d %d" % (l, c, le, ce))

        before_end_pos = line_pos(len(before) - le) + len(before[-le]) - ce
        _undo_to(changenr)
        #vim.current.buffer[:] = after
        after_end_pos = line_pos(len(after) - le) + len(after[-le]) - ce
        remove_len = before_end_pos - start_pos

        #print("we got here, %d %d %d ... %d %d %d / %d %d %d" %
            #(l, c, start_pos,
            #len(before) - le, len(before[-le]) - ce, before_end_pos,
            #len(after) - le, len(after[-le]) - ce, after_end_pos))
        
        if len(after) - le == l:
            #print("QQQ %s %s %s %s %s" % (repr(after[l]), c, len(after[l]), ce, len(after[l]) - ce))
            insert_text = after[l][c:len(after[l])-ce]
        else:
            insert_text = vim.current.buffer[l][c:] + "\n"
            for i in range(l + 1, len(after) - le):
                insert_text += after[i] + "\n"
            insert_text += after[-le][:-ce]
        #print("inserting %s" % repr(insert_text))

    #print("restoring cursor pos: %s buffer == %s" % (repr(cursor_pos), vim.current.buffer[:]))
    vim.current.window.cursor = cursor_pos
    #print("restored cursor pos")
    doc_id = current_doc_id()
    request = {
        "jsonrpc": "2.0",
        "method": "edit",
        "params": {
            "doc_id": doc_id,
            "stamp": changenr,
            "start_pos": start_pos,
            "remove_len": remove_len,
            "insert_text": insert_text,
        },
        "id": 0,
    }
    result = malk_call(request)
    ll = result["full_text"].split("\n")
    if ll != after[:]:
        #print("ll == %s" % ll)
        #print("after[:] == %s" % after)
        assert false
    b.synced_changenr = changenr

malk_init()

EOF
endfunction

function! MalkNormalise()
python << EOF
malk_normalise()
EOF
endfunction

function! MalkEdit()
python << EOF
malk_edit()
EOF
endfunction

function! MalkLoad()
python <<EOF
malk_load()
EOF
endfunction

call MalkInit()

command! -nargs=0 Normalise call MalkNormalise()
nnoremap <buffer> <LocalLeader>n :Normalise<CR>

command! -nargs=0 Edit call MalkEdit()
"au TextChanged,TextChangedI  *.malk call MalkEdit()
"au TextChanged,TextChangedI  *.malk call MalkEdit()
au TextChanged,InsertLeave *.malk call MalkEdit()
"au TextChanged,TextChangedI  *.malk :echo 'wow'
nnoremap <buffer> <LocalLeader>l :Edit<CR>
"autocmd TextChanged,TextChangedI  *.malk call MalkEdit()
"autocmd TextChangedI *.malk 

"autocmd BufRead,BufNewFile *.malk call MalkLoad()
"autocmd BufReadPost,BufNewFile *.malk call MalkLoad()

