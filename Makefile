taxcalc.cgi: taxcalc.cbl
	cobc -o taxcalc.cgi -x $^

install:
	strip taxcalc.cgi
	sudo -u apache -g apache cp taxcalc.cgi /srv/httpd/cgi-bin
	sudo -u apache -g apache cp schedules/* /srv/httpd/cgi-bin/schedules
