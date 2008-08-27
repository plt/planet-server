#! /usr/bin/env python

#xml.py- an XML-RPC client for Trac's xml-rpc plugin. Allows many low-level functions to be executed via commandline
#Note: more functionality could be added to this; this code is sufficient for current uses but 
#it would not be difficult to add more of XML-RPC's supported functions to this collection
import xmlrpclib

#string->server
def getserver(url):
	server = xmlrpclib.ServerProxy(url)
	return server

#server string string dictionary->int
def ticket_create(server, summary, description, attributes={}):
	ticketid = server.ticket.create(summary,discription,attributes)
	return ticketid

#gets an array of all ticket severities
#server->array
def ticket_severity_getAll(server):
	return server.ticket.severity.getAll()

#null->array
def ticket_type_getAll(server):
	return server.ticket.type.getAll()

#to avoid overload, can make one large request out of several; the notation of array methods: {'methodName':string, 'params': array}
#server array->array
def system_multicall(server,methodcalls):
	return server.system.multicall
	
#server->array	
def system_listMethods(server):
	return server.system.listMethods()

#server string ->string
def system_methodHelp(server, method):
	return server.system.methodHelp(method)

#server->list
def ticket_component_getAll(server):
	return server.ticket.component()

#server string ->list
def ticket_query(server, queries):
	return server.ticket.query(queries)

#server dateTime_iso8601 ->list
def ticket_getRecentChanges(server,time):
	return server.ticket.getRecentChanges(time)

#server int ->list
def ticket_getAvailableActions(server,id):
	return server.ticket.getAvailableActions(id)

#server int ->[id, time_created, time_changed, attributes]
def ticket_get(server, int):
	return server.ticket.get(int)

#server int ->listof (time, author, field, oldvalue, newvalue, permanent)
def ticket_changeLog(server, id, when=0):
	return server.ticket.changeLog(id, when)

