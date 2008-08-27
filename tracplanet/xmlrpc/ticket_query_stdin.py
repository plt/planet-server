#! /usr/bin/env python

import xmlrpclib
import sys




server=xmlrpclib.ServerProxy(sys.stdin.readline())
print server.ticket.query(sys.stdin.readline())

