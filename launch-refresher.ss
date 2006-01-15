(module launch-refresher mzscheme
  
  (require "refresh-server.ss"
           (lib "process.ss"))

  (define (restart-web-server response)
    (system "/etc/init.d/plt-web-server stop ; /etc/init.d/plt-web-server start"))
  
  (monitor
   "jacobm@cs.uchicago.edu"
   "planet.plt-scheme.org"
   80
   (* 5 60)
   30
   restart-web-server))
