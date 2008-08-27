#! /usr/bin/env python

import xmlrpclib
import sys

if len(sys.argv)!=3:
	sys.exit("Must provide a server and a methodname.")
server=xmlrpclib.ServerProxy(sys.argv[1])
print server.system.methodHelp(sys.argv[2])
