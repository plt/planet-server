from optparse import OptionParser
# The md5 module is deprecated in Python 2.5
try:
    from hashlib import md5
except ImportError:
    from md5 import md5

# build the options
usage = "usage: %prog [options]"
parser = OptionParser(usage=usage)
parser.add_option("-u", "--username",action="store", dest="username", type = "string",
                  help="the username for whom to generate a password")
parser.add_option("-p", "--password",action="store", dest="password", type = "string",
                  help="the password to use")
parser.add_option("-m","--md5",action="store", dest="md5", type = "string")
(options, args) = parser.parse_args()

# check options
if (options.username is None) or (options.password is None):
   parser.error("You must supply both the username and password")
   
# Generate the string to enter into the htdigest file
realm = 'trac'
if (options.md5 is None):
	kd = lambda x: md5(':'.join(x)).hexdigest()
	print ':'.join((options.username, realm, kd([options.username, realm, options.password])))
else:
	print ':'.join((options.username, realm, options.password))

