#! /usr/bin/env python

import xmlrpclib
import sys

if len(sys.argv)!=2:
	sys.exit("Must provide a server string!")

server=xmlrpclib.ServerProxy(sys.argv[1])
severities=server.ticket.severity.getAll()
print severities
