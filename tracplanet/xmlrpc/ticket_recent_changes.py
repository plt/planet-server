#! /usr/bin/env python
#prints a list of ticket ids which have changed since given timestamp

import xmlrpclib
import sys
import mxDateTime

if len(sys.argv)!=3:
	sys.exit("Must provide a server and a time is8601")

server=xmlrpclib.ServerProxy(sys.argv[1])
date=mxDateTime.Parser.DateFromString(argv[2])
print server.ticket.getRecentChanges(date)
