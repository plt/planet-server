#!/local/pythont/bin/python


import xmlrpclib,sys,getopt

if len(sys.argv)<4:
	sys.exit("Must provide a server, summary, and description with optional attributes")

attributes={}
args=sys.argv[1:]
options,args=getopt.getopt(args,'',['cc=','component=','owner=', 'type=', 'reporter=', 'owner=', 'keywords=', 'version=', 'priority=','severity=', 'milestone=', 'status=', 'resolution='])
for (key,val) in options:
	keyn=key.lstrip('-')
	attributes[keyn]=val


server=xmlrpclib.ServerProxy(args[0])
print server.ticket.create(args[1],args[2],attributes)
