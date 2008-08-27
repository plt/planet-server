#! /usr/bin/env python

import xmlrpclib
import sys

if len(sys.argv)!=2:
	sys.exit("Must provide a server")

server=xmlrpclib.ServerProxy(sys.argv[1])
print server.system.listMethods()
