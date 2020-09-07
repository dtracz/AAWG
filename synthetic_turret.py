#!/usr/bin/env python3

import http.server
import socketserver

PORT = 3100

class ServerHandler(http.server.SimpleHTTPRequestHandler):
    def do_POST(self):
        lgh = int(self.headers.get('content-length', 0))
        print(self.rfile.read(lgh))
        http.server.SimpleHTTPRequestHandler.do_GET(self)

with socketserver.TCPServer(("", PORT), ServerHandler) as httpd:
    print("serving at port", PORT)
    httpd.serve_forever()

