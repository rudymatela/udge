# Makefile for Udge
#
#
# Copyright (C) 2015-2020  Rudy Matela
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
#
# This Makefile uses GNU make extensions.

PREFIX        = /usr/local
# HTTPD_USER is guessed to be www-data, http, nginx, www or root in that order.
HTTPD_USER    = $(shell id -u www-data -n 2>/dev/null || \
                        id -u http     -n 2>/dev/null || \
                        id -u nginx    -n 2>/dev/null || \
                        id -u www      -n 2>/dev/null || \
                        id -u root     -n 2>/dev/null)
# NGINX_AVAIL is guessed to be sites-available and srv/avail in that order.
NGINX_AVAIL   = $(shell \
	[ -d /etc/nginx/sites-available ] && echo /etc/nginx/sites-available || \
	[ -d /etc/nginx/srv/avail ]       && echo /etc/nginx/srv/avail       || \
	echo /etc/udge-nginx-example)
# NGINX_ENABLED is guessed to be sites-enabled and srv/enabled in that order.
NGINX_ENABLED = $(shell \
	[ -d /etc/nginx/sites-enabled ]   && echo /etc/nginx/sites-enabled || \
	[ -d /etc/nginx/srv/enabled ]     && echo /etc/nginx/srv/enabled   || \
	true)
TIDY          = tidy -qe --show-filename yes

.PHONY: all
all:

clean: \
	clean-html \
	clean-submissions \
	clean-test-users

realclean: \
	clean \
	clean-results \
	clean-users \
	clean-var

fastest:
	make test-scripts -j7
	make test-web

test: \
  test-scripts \
  test-web

test-scripts: export DEFAULT_TIME_LIMIT=2
test-scripts: \
  test-makefile \
  test-no-broken-links \
  tidy \
  judge.clitest \
  hello-world.clitest \
  hello-world-hs.clitest \
  hello-world-py.clitest \
  hello-world-cc.clitest \
  hello-world-cs.clitest \
  hello-world-java.clitest \
  hello-world-js.clitest \
  addition.clitest \
  addition-hs.clitest \
  addition-py.clitest \
  addition-cs.clitest \
  addition-js.clitest \
  add.clitest \
  add-hs.clitest \
  add-py.clitest \
  add-cc.clitest \
  add-cs.clitest \
  add-java.clitest \
  add-js.clitest \
  rectangle.clitest \
  rectangle-hs.clitest \
  rectangle-py.clitest \
  rectangle-cc.clitest \
  rectangle-cs.clitest \
  rectangle-java.clitest \
  rectangle-js.clitest \
  cat.clitest \
  sandbox.clitest \
  hello.clitest

# these cannot be run in parallel
test-web: \
  index.clitest \
  sandbox-fork.clitest \
  test-web-noindex

test-web-noindex: \
  new-user.clitest \
  new-user-toggle.clitest \
  names.clitest \
  submit.clitest \
  submit-rate.clitest \
  test-happy

test-happy: \
  happy-day-1.clitest \
  happy-day-2.clitest \
  happy-day-3.clitest

test-no-broken-links: html
	wget -nv -r udge/
	rm -r udge/

test-makefile: test-makefile-coverage test-install test-dev-install

test-makefile-coverage:
	rm -f /tmp/udge-clitests /tmp/udge-txts
	cat Makefile  | grep -E '[-a-z0-9]+\.clitest' | sed -e 's/ *//g; s/.clitest.*//' | sort > /tmp/udge-clitests
	ls -1 examples/*.txt | sed -e 's,examples/,,; s,.txt,,'                          | sort > /tmp/udge-txts
	diff -rud /tmp/udge-clitests /tmp/udge-txts
	rm -f /tmp/udge-clitests /tmp/udge-txts

.PHONY: %.clitest
%.clitest: examples/%.txt
	PATH="./bin:$$PATH" clitest -1 $<

html: readme todo robots
	./bin/udge-update-all-problem-htmls
	./bin/udge-update-all-user-htmls
	./bin/udge-update-rank-html

html-force: readme todo robots
	./bin/udge-update-all-problem-htmls force
	./bin/udge-update-all-user-htmls force
	./bin/udge-update-rank-html

clean-html:
	rm -rf var/html

%.tidy: html
	curl -sL $* | $(TIDY)

.PHONY: readme
readme: var/html/README.html

.PHONY: todo
todo: var/html/TODO.html

.PHONY: robots
robots: var/html/robots.txt

var/html/README.html: README.md
	./lib/udge/markdown $< > $@

var/html/TODO.html: TODO.md
	./lib/udge/markdown $< > $@

var/html/robots.txt:
	echo -e 'User-agent: *\nAllow: /' > $@

tidy: \
	udge.tidy \
	udge/submit.tidy \
	udge/new-user.tidy \
	udge/hello.tidy \
	udge/hello-world.tidy \
	udge/add.tidy \
	udge/rank.tidy \
	tidy-public_html

tidy-public_html: html
	for file in `find var/html -name *.html`; do \
		$(TIDY) "$$file" || break; done

clean-submissions:
	rm -rf var/submissions/*

clean-slots:
	rm -rf var/slot/*/lock
	rm -rf /run/udge/*/lock

clean-results:
	rm -rf var/results

clean-users:
	rm -rf var/users/*

clean-var:
	rm -rf var

clean-test-users:
	rm -rf /var/lib/udge/users/test-*-*-*
	rm -rf /var/lib/udge/results/test-*-*-*
	rm -rf /var/lib/udge/submissions/test-*-*-*
	rm -rf /var/lib/udge/trial/test-*-*-*
	rm -rf /var/lib/udge/slot/test-*-*-*
	rm -rf /var/lib/udge/html/u/test-*-*-*.html

install:
	mkdir -p $(DESTDIR)/etc
	mkdir -p $(DESTDIR)$(NGINX_AVAIL)
	mkdir -p $(DESTDIR)/var/lib
	mkdir -p $(DESTDIR)$(PREFIX)/lib
	mkdir -p $(DESTDIR)$(PREFIX)/bin
	mkdir -p $(DESTDIR)$(PREFIX)/cgi-bin
	install -m 0644 etc/udgerc $(DESTDIR)/etc/udgerc
	install -m 0644 etc/nginx/srv/avail/udge $(DESTDIR)$(NGINX_AVAIL)/udge
	install -m 0755 -d $(DESTDIR)/var/lib/udge
	install -m 2770 -d $(DESTDIR)/var/lib/udge/users
	install -m 0755 -d $(DESTDIR)/var/lib/udge/problem
	install -m 0755 -d $(DESTDIR)/var/lib/udge/html
	install -m 2775 -d $(DESTDIR)/var/lib/udge/submissions
	install -m 0755 -d $(DESTDIR)/var/lib/udge/trial
	install -m 0755 -d $(DESTDIR)/var/lib/udge/slot
	install -m 0755 -d $(DESTDIR)/var/lib/udge/slot/1
	install -m 0755 -d $(DESTDIR)/var/lib/udge/slot/2
	install -m 0755 -d $(DESTDIR)/var/lib/udge/slot/3
	install -m 0755 -d $(DESTDIR)/var/lib/udge/slot/4
	install -m 0755 -d $(DESTDIR)/var/lib/udge/slot/5
	install -m 0755 -d $(DESTDIR)/var/lib/udge/slot/6
	install -m 0755 -d $(DESTDIR)/var/lib/udge/results
	install -m 0755 bin/cgi-create-data-files         $(DESTDIR)$(PREFIX)/bin
	install -m 0755 bin/udge-add-user                 $(DESTDIR)$(PREFIX)/bin
	install -m 0755 bin/udge-backup                   $(DESTDIR)$(PREFIX)/bin
	install -m 0755 bin/udge-check                    $(DESTDIR)$(PREFIX)/bin
	install -m 0755 bin/udge-compile-and-run          $(DESTDIR)$(PREFIX)/bin
	install -m 0755 bin/udge-create-run               $(DESTDIR)$(PREFIX)/bin
	install -m 0755 bin/udge-delete-user              $(DESTDIR)$(PREFIX)/bin
	install -m 0755 bin/udge-judge                    $(DESTDIR)$(PREFIX)/bin
	install -m 0755 bin/udge-latest-results           $(DESTDIR)$(PREFIX)/bin
	install -m 0755 bin/udge-passwd                   $(DESTDIR)$(PREFIX)/bin
	install -m 0755 bin/udge-pick                     $(DESTDIR)$(PREFIX)/bin
	install -m 0755 bin/udge-pick-and-judge           $(DESTDIR)$(PREFIX)/bin
	install -m 0755 bin/udge-rank                     $(DESTDIR)$(PREFIX)/bin
	install -m 0755 bin/udge-sandbox                  $(DESTDIR)$(PREFIX)/bin
	install -m 0755 bin/udge-submit                   $(DESTDIR)$(PREFIX)/bin
	install -m 0755 bin/udge-update-all-problem-htmls $(DESTDIR)$(PREFIX)/bin
	install -m 0755 bin/udge-update-all-user-htmls    $(DESTDIR)$(PREFIX)/bin
	install -m 0755 bin/udge-update-rank-html         $(DESTDIR)$(PREFIX)/bin
	install -m 0755 bin/udge-update-user-html         $(DESTDIR)$(PREFIX)/bin
	install -m 0755 bin/udge-user-stats               $(DESTDIR)$(PREFIX)/bin
	install -m 0755 cgi-bin/udge-new-user $(DESTDIR)$(PREFIX)/cgi-bin
	install -m 0755 cgi-bin/udge-submit   $(DESTDIR)$(PREFIX)/cgi-bin
	install -m 0755 -d                         $(DESTDIR)$(PREFIX)/lib/udge
	install -m 0644 lib/udge/bootstrap.css     $(DESTDIR)$(PREFIX)/lib/udge
	install -m 0644 lib/udge/html              $(DESTDIR)$(PREFIX)/lib/udge
	install -m 0644 lib/udge/cgi               $(DESTDIR)$(PREFIX)/lib/udge
	install -m 0644 lib/udge/core              $(DESTDIR)$(PREFIX)/lib/udge
	install -m 0755 lib/udge/jarvac            $(DESTDIR)$(PREFIX)/lib/udge
	install -m 0755 lib/udge/markdown          $(DESTDIR)$(PREFIX)/lib/udge
	install -m 0755 lib/udge/rank-html         $(DESTDIR)$(PREFIX)/lib/udge
	install -m 0755 lib/udge/user-html         $(DESTDIR)$(PREFIX)/lib/udge
	install -m 0755 lib/udge/check             $(DESTDIR)$(PREFIX)/lib/udge
	install -m 0755 lib/udge/check-1           $(DESTDIR)$(PREFIX)/lib/udge
	install -m 0755 lib/udge/compile-and-run   $(DESTDIR)$(PREFIX)/lib/udge
	install -m 0755 lib/udge/compile-and-run-1 $(DESTDIR)$(PREFIX)/lib/udge
	install -m 0755 -d                    $(DESTDIR)$(PREFIX)/lib/udge/compile
	install -m 0755 lib/udge/compile/c    $(DESTDIR)$(PREFIX)/lib/udge/compile
	install -m 0755 lib/udge/compile/hs   $(DESTDIR)$(PREFIX)/lib/udge/compile
	install -m 0755 lib/udge/compile/py   $(DESTDIR)$(PREFIX)/lib/udge/compile
	install -m 0755 lib/udge/compile/cc   $(DESTDIR)$(PREFIX)/lib/udge/compile
	install -m 0755 lib/udge/compile/cs   $(DESTDIR)$(PREFIX)/lib/udge/compile
	install -m 0755 lib/udge/compile/java $(DESTDIR)$(PREFIX)/lib/udge/compile
	install -m 0755 lib/udge/compile/js   $(DESTDIR)$(PREFIX)/lib/udge/compile
	install -m 0755 -d                           $(DESTDIR)$(PREFIX)/lib/udge/compile-as-lib
	install -m 0755 lib/udge/compile-as-lib/c    $(DESTDIR)$(PREFIX)/lib/udge/compile-as-lib
	install -m 0755 lib/udge/compile-as-lib/hs   $(DESTDIR)$(PREFIX)/lib/udge/compile-as-lib
	install -m 0755 lib/udge/compile-as-lib/py   $(DESTDIR)$(PREFIX)/lib/udge/compile-as-lib
	install -m 0755 lib/udge/compile-as-lib/cc   $(DESTDIR)$(PREFIX)/lib/udge/compile-as-lib
	install -m 0755 lib/udge/compile-as-lib/cs   $(DESTDIR)$(PREFIX)/lib/udge/compile-as-lib
	install -m 0755 lib/udge/compile-as-lib/java $(DESTDIR)$(PREFIX)/lib/udge/compile-as-lib
	install -m 0755 lib/udge/compile-as-lib/js   $(DESTDIR)$(PREFIX)/lib/udge/compile-as-lib
	install -m 0755 -d                       $(DESTDIR)$(PREFIX)/lib/udge/score
	install -m 0755 lib/udge/score/fractions $(DESTDIR)$(PREFIX)/lib/udge/score
	install -m 0755 lib/udge/score/icpc      $(DESTDIR)$(PREFIX)/lib/udge/score
	install -m 0755 lib/udge/score/solved    $(DESTDIR)$(PREFIX)/lib/udge/score
	install -m 0755 lib/udge/score/sum       $(DESTDIR)$(PREFIX)/lib/udge/score
	cp -r problem/* $(DESTDIR)/var/lib/udge/problem
	[ "$$EUID" -ne 0 ] || id -u udge   >/dev/null 2>&1 || useradd -r -d/var/lib/udge -s/usr/bin/nologin udge
	[ "$$EUID" -ne 0 ] || id -u udge-1 >/dev/null 2>&1 || useradd -r -d/run/udge -s/usr/bin/nologin udge-1
	[ "$$EUID" -ne 0 ] || id -u udge-2 >/dev/null 2>&1 || useradd -r -d/run/udge -s/usr/bin/nologin udge-2
	[ "$$EUID" -ne 0 ] || id -u udge-3 >/dev/null 2>&1 || useradd -r -d/run/udge -s/usr/bin/nologin udge-3
	[ "$$EUID" -ne 0 ] || id -u udge-4 >/dev/null 2>&1 || useradd -r -d/run/udge -s/usr/bin/nologin udge-4
	[ "$$EUID" -ne 0 ] || id -u udge-5 >/dev/null 2>&1 || useradd -r -d/run/udge -s/usr/bin/nologin udge-5
	[ "$$EUID" -ne 0 ] || id -u udge-6 >/dev/null 2>&1 || useradd -r -d/run/udge -s/usr/bin/nologin udge-6
	# Notes on permissions and ownership.
	#
	# CGI scripts need permission to create entries on users/ and submissions/
	# so they are set to be owned by the HTTPd user.
	#
	# pick-and-judge needs permission to delete entries from submissions/ so
	# this directory is set with the setgid flag (2775) and the udge group
	# which makes files created within it inhehit the group and allows deletion
	# by the udge user.
	#
	# html, slot and results need to be writable by the udge user.
	[ "$$EUID" -ne 0 ] || chown $(HTTPD_USER).udge $(DESTDIR)/var/lib/udge/users
	[ "$$EUID" -ne 0 ] || chown $(HTTPD_USER).udge $(DESTDIR)/var/lib/udge/submissions
	[ "$$EUID" -ne 0 ] || chown    udge.udge $(DESTDIR)/var/lib/udge/html
	[ "$$EUID" -ne 0 ] || chown    udge.udge $(DESTDIR)/var/lib/udge/trial
	[ "$$EUID" -ne 0 ] || chown -R udge.udge $(DESTDIR)/var/lib/udge/slot
	[ "$$EUID" -ne 0 ] || chown    udge.udge $(DESTDIR)/var/lib/udge/results

# Use with care.  This can potentially delete more than wanted.
uninstall:
	for file in `find bin lib cgi-bin -type f`; do \
		rm -f $(DESTDIR)$(PREFIX)/$$file; done
	rm -rf $(DESTDIR)$(PREFIX)/lib/udge

# Use with care.  This can potentially delete more than wanted.
now=$(shell date "+%Y%m%d-%H%M%S")
uninstall-and-purge: uninstall
	mv $(DESTDIR)/etc/udgerc{,-old-$(now)}
	mv $(DESTDIR)$(NGINX_AVAIL)/udge{,-old-$(now)}
	mv $(DESTDIR)/var/lib/udge{,-old-$(now)}
	userdel udge

# Run this as your regular user before dev-install
dev-setup:
	install -m 0755 -d var
	install -m 2770 -d var/users
	install -m 2775 -d var/submissions
	install -m 0755 -d var/slot
	install -m 0755 -d var/slot/1
	install -m 0755 -d var/slot/2
	install -m 0755 -d var/slot/3
	install -m 0755 -d var/slot/4
	install -m 0755 -d var/slot/5
	install -m 0755 -d var/slot/6
	ln -rsfT problem var/problem

# Run this as root after dev-setup
dev-install:
	mkdir -p                         $(DESTDIR)/etc
	mkdir -p                         $(DESTDIR)$(NGINX_AVAIL)
	mkdir -p                         $(DESTDIR)/var/lib
	ln -sfT `pwd`/etc/udgerc         $(DESTDIR)/etc/udgerc
	ln -sfT `pwd`$(NGINX_AVAIL)/udge $(DESTDIR)$(NGINX_AVAIL)/udge
	mkdir -p var
	ln -sfT `pwd`/var                $(DESTDIR)/var/lib/udge
	for dir in `find bin lib cgi-bin -type d`; do \
		mkdir -p $(DESTDIR)$(PREFIX)/$$dir; done
	for file in `find bin lib cgi-bin -type f`; do \
		ln -sf `pwd`/$$file $(DESTDIR)$(PREFIX)/$$file; done
	[ "$$EUID" -ne 0 ] || chown $(HTTPD_USER) $(DESTDIR)/var/lib/udge/users
	[ "$$EUID" -ne 0 ] || chown $(HTTPD_USER) $(DESTDIR)/var/lib/udge/submissions

start-services:
	udge-create-run
	systemctl start fcgiwrap.socket
	systemctl start fcgiwrap
	systemctl start nginx

stop-services:
	systemctl stop nginx
	systemctl stop fcgiwrap.socket
	systemctl stop fcgiwrap

enable-nginx-udge-site:
	[ -n "$(NGINX_ENABLED)" ]
	ln -rs $(NGINX_AVAIL)/udge $(NGINX_ENABLED)/udge
	systemctl reload nginx


test-install:
	[ ! -e pkg/i ]
	make install       DESTDIR=pkg/i
	make check-install DESTDIR=pkg/i
	make uninstall     DESTDIR=pkg/i
	find pkg/i -type f
	find pkg/i -type f | wc -l
	[ "`find pkg/i -type f | wc -l`" -eq 86 ] # udgerc, nginx conf and problems
	rm -r pkg/i
	rmdir pkg || true

misc:
	var=`mktemp -d /tmp/misc-makefile-XXXXXXXXXX`; echo $$var

test-dev-install:
	[ ! -e pkg/d ]
	make dev-install        DESTDIR=pkg/d
	make check-install-test DESTDIR=pkg/d
	rm -r pkg/d
	rmdir pkg || true

list-missing-copyright:
	grep -LR Copyright bin/ cgi-bin/ COPYING etc/ examples/ lib/ LICENSE Makefile README.md || true

cloc:
	cloc bin/ cgi-bin/ COPYING etc/ examples/ lib/ LICENSE Makefile problem/ README.md TODO.md

# NOTE: this only works on an "empty" tree.
# Do not use this target to check a real install.
check-install: check-install-test check-install-find

check-install-test:
	diff -rud lib     $(DESTDIR)$(PREFIX)/lib
	diff -rud bin     $(DESTDIR)$(PREFIX)/bin
	diff -rud cgi-bin $(DESTDIR)$(PREFIX)/cgi-bin
	diff -rud etc/udgerc $(DESTDIR)/etc/udgerc
	[ -z "$(NGINX_ENABLED)" ] || \
	diff -rud $(NGINX_AVAIL)/udge $(NGINX_ENABLED)/udge
	[ -d $(DESTDIR)/var/lib/udge ]

check-install-find:
	echo $(DESTDIR) $(PREFIX)
	find pkg -type f \
	| sed -e "s,$(DESTDIR),,;s,$(PREFIX),,;s,/var/lib/udge,,;s,^/,," \
	| sort > installed-files.txt
	find lib bin cgi-bin etc problem -type f \
	| sort > installable-files.txt
	diff -rud install{able,ed}-files.txt
	rm install{able,ed}-files.txt

show-vars:
	@echo "PREFIX        = $(PREFIX)"
	@echo "HTTPD_USER    = $(HTTPD_USER)"
	@echo "NGINX_AVAIL   = $(NGINX_AVAIL)"
	@echo "NGINX_ENABLED = $(NGINX_ENABLED)"
	@echo "TIDY          = $(TIDY)"
