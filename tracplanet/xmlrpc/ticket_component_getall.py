#! /usr/bin/env python

import xmlrpclib
import sys

if len(sys.argv)!=2:
	sys.exit("Need a server as an argument.")

server=xmlrpclib.ServerProxy(sys.argv[1])
print server.ticket.component.getAll()

