#!/usr/bin/env python3

from http.server import HTTPServer, BaseHTTPRequestHandler
import json
from loguru import logger

class MyHandler(BaseHTTPRequestHandler):
    def do_GET(self):
        """Handle GET requests"""
        # pass

        # Add response status code.
        if self.path == "/404":
            self.send_response(404)
            self.end_headers()
            return
        elif self.path == "/408":
            self.send_response(408)
            self.end_headers()
            return
        elif self.path == "/502":
            self.send_response(502)
            self.end_headers()
            return
        else:
            self.send_response(200)

        self.protocol_version = "HTTP/1.1"

        # Add response headers.
        self.send_header('Content-Type', 'application/json; charset=utf-8')
        self.end_headers()

        # Add response content.
        response_content = json.dumps({"code": 200, "msg": "test"})
        self.wfile.write(response_content.encode('utf-8'))

    def do_POST(self):
        """Handle POST requests"""
        # pass

        # Add response status code.
        if self.path == "/404":
            self.send_response(404)
            self.end_headers()
            return
        elif self.path == "/408":
            self.send_response(408)
            self.end_headers()
            return
        elif self.path == "/502":
            self.send_response(502)
            self.end_headers()
            return
        else:
            self.send_response(200)

        self.protocol_version = "HTTP/1.1"

        # Add response headers.
        self.send_header('Content-Type', 'application/json; charset=utf-8')
        self.end_headers()

        # Add response content.
        response_content = json.dumps({"code": 200, "msg": "test"})
        self.wfile.write(response_content.encode('utf-8'))

    def log_request(self, code=None, size=None):
        """Don't log anything"""
        if self.path != "/favicon.ico":
            logger.info("path:{}", self.path)

if __name__ == '__main__':
    httpd = HTTPServer(('localhost', 9999), MyHandler)
    httpd.serve_forever()

