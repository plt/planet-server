#! /usr/bin/env python

import xmlrpclib
import sys

if len(sys.argv)<3:
	sys.exit("Must provide server url")
server=xmlrpclib.ServerProxy(sys.argv[1])
if len(sys.argv)==3:
	print server.ticket.changeLog(sys.argv[2])
else :
	print server.ticket.changeLog(sys.argv[2].sys.argv[3])
	
