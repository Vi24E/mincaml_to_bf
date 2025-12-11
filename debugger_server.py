#!/usr/bin/env python3
import http.server
import socketserver
import json
import sys
import os
import urllib.parse
from debugger import Debugger

PORT = 8000
DBG_INSTANCE = None

class DebuggerHandler(http.server.SimpleHTTPRequestHandler):
    def do_GET(self):
        global DBG_INSTANCE
        
        parsed_path = urllib.parse.urlparse(self.path)
        
        if parsed_path.path == '/state':
            self.send_response(200)
            self.send_header('Content-type', 'application/json')
            self.end_headers()
            
            # Serialize state
            # Code ops are objects, need string representation
            ops_str = [str(op) for op in DBG_INSTANCE.ops]
            
            # Memory window around ptr
            tape_len = len(DBG_INSTANCE.tape)
            
            # We send simplified tape for checking, or full? 
            # Sending full tape (30k) might be heavy every frame if we poll fast.
            # But local network is fast. Let's send a window or specific requested regions.
            # actually frontend might want to scroll.
            # For now, let's send requested regions if params present, otherwise safe defaults.
            
            response = {
                'pc': DBG_INSTANCE.pc,
                'ptr': DBG_INSTANCE.ptr,
                'step_count': DBG_INSTANCE.step_count,
                'finished': DBG_INSTANCE.pc >= len(DBG_INSTANCE.ops),
                'tape_size': tape_len,
                # Metadata
                'reg_start': DBG_INSTANCE.reg_start,
                'buffer_start': DBG_INSTANCE.buffer_start,
                'var_start': DBG_INSTANCE.var_start,
                'stack_start': DBG_INSTANCE.stack_start,
                'context': DBG_INSTANCE.get_active_context(),
                'variables': DBG_INSTANCE.get_variables(),
            }
            
            # If initial load (or requested), send ops
            query = urllib.parse.parse_qs(parsed_path.query)
            if 'init' in query:
                response['ops'] = ops_str
            
            # Memory chunks
            # We can support requesting multiple chunks
            # For now, let's just send the "standard" chunks we care about
            
            chunks = {}
            
            # 1. Around PTR (Local Tape)
            window = 64
            start_local = max(0, DBG_INSTANCE.ptr - 32)
            end_local = min(tape_len, start_local + 64)
            chunks['local_tape'] = {
                'start': start_local,
                'data': DBG_INSTANCE.tape[start_local:end_local]
            }
            
            # 2. Registers (if metadata)
            if DBG_INSTANCE.reg_start is not None:
                chunks['registers'] = {
                    'start': DBG_INSTANCE.reg_start,
                    'data': DBG_INSTANCE.tape[DBG_INSTANCE.reg_start:DBG_INSTANCE.reg_start + 128]
                }
                
            # 3. Buffer (if metadata)
            if DBG_INSTANCE.buffer_start is not None:
                chunks['buffer'] = {
                    'start': DBG_INSTANCE.buffer_start,
                    'data': DBG_INSTANCE.tape[DBG_INSTANCE.buffer_start:DBG_INSTANCE.buffer_start + 32]
                }
                
            # 4. Stack (if metadata)
            # 4. Stack (if metadata)
            if DBG_INSTANCE.stack_start is not None:
                # Send raw chunk (optional, maybe frontend wants it?)
                # And send parsed values
                stack_size = 256
                chunks['stack'] = {
                    'start': DBG_INSTANCE.stack_start,
                    'data': DBG_INSTANCE.tape[DBG_INSTANCE.stack_start:DBG_INSTANCE.stack_start + stack_size]
                }
                # Add parsed stack values to response root or under memory?
                # Let's add to root like variables
                response['stack_values'] = DBG_INSTANCE.get_stack_values(limit=32)

            # 5. Activated Cells (Block Flags)
            if DBG_INSTANCE.reg_start is not None:
                chunks['activated_cells'] = {
                    'start': 0,
                    'data': DBG_INSTANCE.tape[0:DBG_INSTANCE.reg_start]
                }

            response['memory'] = chunks
            
            self.wfile.write(json.dumps(response).encode())
            return
            
        # Serve static files from debugger_web directory
        # Map / to index.html
        if self.path == '/':
            self.path = '/index.html'
            
        # Adjust path to serve from debugger_web subdirectory
        # Note: SimpleHTTPRequestHandler serves from current directory by default
        # We want to serve from ./mincaml_to_bf/debugger_web/ usually?
        # Or we act as if root is debugger_web
        
        # Current working directory when running script might be different.
        # Let's set directory to serve from
        f = self.send_head()
        if f:
            self.copyfile(f, self.wfile)
            f.close()

    def translate_path(self, path):
        # Override to serve from debugger_web directory relative to script
        path = super().translate_path(path)
        # Get the path relative to CWD
        rel_path = os.path.relpath(path, os.getcwd())
        
        # Logic: 
        # If we are running from cpu実験/
        # script is in mincaml_to_bf/debugger_server.py
        # Web files are in mincaml_to_bf/debugger_web/
        
        script_dir = os.path.dirname(os.path.abspath(__file__))
        web_root = os.path.join(script_dir, 'debugger_web')
        
        # We just want to map URL path to physical path inside web_root
        # super().translate_path already does some mapping, but essentially we want to re-root
        
        # Simpler approach:
        # Just manually construct path
        p = parsed_path = urllib.parse.urlparse(self.path)
        path = p.path
        if path == '/':
            path = '/index.html'
            
        # Security check: don't allow traversal up
        path = path.lstrip('/')
        full_path = os.path.join(web_root, path)
        return full_path

    def do_POST(self):
        global DBG_INSTANCE
        if self.path == '/step':
            content_length = int(self.headers['Content-Length'])
            post_data = self.rfile.read(content_length)
            data = json.loads(post_data)
            
            count = data.get('count', 1)
            
            steps_done = 0
            for _ in range(count):
                if not DBG_INSTANCE.run_step():
                    break
                steps_done += 1
                
            self.send_response(200)
            self.send_header('Content-type', 'application/json')
            self.end_headers()
            self.wfile.write(json.dumps({'steps_executed': steps_done}).encode())
            return
            
        elif self.path == '/step_small':
            DBG_INSTANCE.run_to_tag(2)
            self.send_response(200)
            self.send_header('Content-type', 'application/json')
            self.end_headers()
            self.wfile.write(json.dumps({'status': 'ok'}).encode())
            return
        elif self.path == '/step_large':
            DBG_INSTANCE.run_to_tag(1)
            self.send_response(200)
            self.send_header('Content-type', 'application/json')
            self.end_headers()
            self.wfile.write(json.dumps({'status': 'ok'}).encode())
            return
        elif self.path == '/step_huge':
            count = 100000
            steps_done = 0
            for _ in range(count):
                if not DBG_INSTANCE.run_step():
                    break
                steps_done += 1
            
            self.send_response(200)
            self.send_header('Content-type', 'application/json')
            self.end_headers()
            self.wfile.write(json.dumps({'steps_executed': steps_done}).encode())
            return
        elif self.path == '/reset':
            # Reload file
            # We need to know filename. It was passed in sys.argv
            # Let's simple re-create
            filename = sys.argv[1]
            with open(filename, 'r') as f:
                code = f.read()
            DBG_INSTANCE = Debugger(code)
            
            self.send_response(200)
            self.end_headers()
            self.wfile.write(b'OK')
            return

def run_server(filename):
    global DBG_INSTANCE
    with open(filename, 'r') as f:
        code = f.read()
    DBG_INSTANCE = Debugger(code)
    
    print(f"Starting Web Debugger for {filename}")
    print(f"Open http://localhost:{PORT} in your browser")
    
    with socketserver.TCPServer(("", PORT), DebuggerHandler) as httpd:
        try:
            httpd.serve_forever()
        except KeyboardInterrupt:
            pass

if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Usage: python3 debugger_server.py <bf_file>")
        sys.exit(1)
        
    run_server(sys.argv[1])
