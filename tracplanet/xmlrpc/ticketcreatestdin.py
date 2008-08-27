#!/local/pythont/bin/python
# -*- coding: utf-8 -*-

import codecs
import xmlrpclib,sys,getopt

args = int(sys.stdin.readline())
x = 0 
arguments = []
while x < args:
	maxlen = int(sys.stdin.readline())
	line = sys.stdin.read(maxlen)
	arguments.append(line)
	x = x + 1
attributes = {}
options, realargs = getopt.getopt(arguments, '',['cc=','component=','owner=', 'type=', 'reporter=', 'owner=', 'keywords=', 'version=', 'priority=','severity=', 'milestone=', 'status=', 'resolution='])
for (key,val) in options:
	keyn=key.lstrip('-')
	attributes[keyn]=val.rstrip('\n').decode('utf-8')
sargs = []
for i in realargs:
	sargs.append(i.rstrip('\n').decode('utf-8'))

server=xmlrpclib.ServerProxy(sargs[0].encode('utf-8'))
print server.ticket.create(sargs[1],sargs[2],attributes)




