#! /usr/bin/env python

import xmlrpclib
import sys

if len(sys.argv)!=3:
	sys.exit("Must provide server and ticket id.")
server=xmlrpclib.ServerProxy(sys.argv[1])
print server.ticket.getAvailableActions(sys.argv[2])
