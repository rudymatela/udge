# Makefile for Udge
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

PREFIX        = /usr/local
HTTPD_USER    = http
NGINX_AVAIL   = /etc/nginx/srv/avail
NGINX_ENABLED = /etc/nginx/srv/enabled
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
	make test-scripts -j
	make test-web

test: \
  tidy \
  test-scripts \
  test-web

test-scripts: \
  test-makefile \
  judge.clitest \
  hello-world.clitest \
  hello-world-hs.clitest \
  hello-world-py.clitest \
  add.clitest \
  add-hs.clitest \
  add-py.clitest \
  attack.clitest \
  hello.clitest

# these cannot be run in parallel
test-web: \
  index.clitest \
  attack-fork.clitest \
  test-web-noindex

test-web-noindex: \
  new-user.clitest \
  new-user-toggle.clitest \
  submit.clitest \
  submit-rate.clitest \
  test-happy

test-happy: \
  happy-day-1.clitest \
  happy-day-2.clitest \
  happy-day-3.clitest

test-makefile: test-makefile-coverage test-dev-install # TODO: add test-install, use mktemp to allow -j

test-makefile-coverage:
	rm -f /tmp/udge-clitests /tmp/udge-txts
	cat Makefile  | grep -E '[-a-z0-9]+\.clitest' | sed -e 's/ *//g; s/.clitest.*//' | sort > /tmp/udge-clitests
	ls -1 examples/*.txt | sed -e 's,examples/,,; s,.txt,,'                          | sort > /tmp/udge-txts
	diff -rud /tmp/udge-clitests /tmp/udge-txts
	rm -f /tmp/udge-clitests /tmp/udge-txts

.PHONY: %.clitest
%.clitest: examples/%.txt
	PATH="./bin:$$PATH" clitest -1 $<

html:
	./bin/udge-update-all-problem-htmls
	./bin/udge-update-all-user-htmls
	./bin/udge-update-rank-html

html-force:
	./bin/udge-update-all-problem-htmls force
	./bin/udge-update-all-user-htmls force
	./bin/udge-update-rank-html

clean-html:
	rm -rf var/html

%.tidy: html
	curl -sL $* | $(TIDY)

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
	for file in `find public_html -name *.html`; do \
		$(TIDY) "$$file" || break; done

clean-submissions:
	rm -rf var/submissions/*

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
	install -m 2775 -d $(DESTDIR)/var/lib/udge/submissions
	install -m 0755 bin/cgi-create-data-files         $(DESTDIR)$(PREFIX)/bin
	install -m 0755 bin/udge-add-user                 $(DESTDIR)$(PREFIX)/bin
	install -m 0755 bin/udge-judge                    $(DESTDIR)$(PREFIX)/bin
	install -m 0755 bin/udge-latest-results           $(DESTDIR)$(PREFIX)/bin
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
	install -m 0644 lib/udge/bootstrap.min.css $(DESTDIR)$(PREFIX)/lib/udge
	install -m 0644 lib/udge/html              $(DESTDIR)$(PREFIX)/lib/udge
	install -m 0644 lib/udge/cgi               $(DESTDIR)$(PREFIX)/lib/udge
	install -m 0644 lib/udge/core              $(DESTDIR)$(PREFIX)/lib/udge
	install -m 0755 lib/udge/markdown          $(DESTDIR)$(PREFIX)/lib/udge
	install -m 0755 lib/udge/rank-html         $(DESTDIR)$(PREFIX)/lib/udge
	install -m 0755 lib/udge/user-html         $(DESTDIR)$(PREFIX)/lib/udge
	install -m 0755 lib/udge/compile-and-test  $(DESTDIR)$(PREFIX)/lib/udge
	install -m 0755 -d                  $(DESTDIR)$(PREFIX)/lib/udge/compile
	install -m 0755 lib/udge/compile/c  $(DESTDIR)$(PREFIX)/lib/udge/compile
	install -m 0755 lib/udge/compile/hs $(DESTDIR)$(PREFIX)/lib/udge/compile
	install -m 0755 lib/udge/compile/py $(DESTDIR)$(PREFIX)/lib/udge/compile
	install -m 0755 -d                         $(DESTDIR)$(PREFIX)/lib/udge/compile-as-lib
	install -m 0755 lib/udge/compile-as-lib/c  $(DESTDIR)$(PREFIX)/lib/udge/compile-as-lib
	install -m 0755 lib/udge/compile-as-lib/hs $(DESTDIR)$(PREFIX)/lib/udge/compile-as-lib
	install -m 0755 lib/udge/compile-as-lib/py $(DESTDIR)$(PREFIX)/lib/udge/compile-as-lib
	install -m 0755 -d                       $(DESTDIR)$(PREFIX)/lib/udge/score
	install -m 0755 lib/udge/score/fractions $(DESTDIR)$(PREFIX)/lib/udge/score
	install -m 0755 lib/udge/score/icpc      $(DESTDIR)$(PREFIX)/lib/udge/score
	install -m 0755 lib/udge/score/solved    $(DESTDIR)$(PREFIX)/lib/udge/score
	install -m 0755 lib/udge/score/sum       $(DESTDIR)$(PREFIX)/lib/udge/score
	[ "$$EUID" -ne 0 ] || id -u udge >/dev/null 2>&1 || useradd -r -d/var/lib/udge -s/usr/bin/nologin udge
	[ -z "$(HTTPD_USER)" ] || chown $(HTTPD_USER) $(DESTDIR)/var/lib/udge/users
	[ -z "$(HTTPD_USER)" ] || chown $(HTTPD_USER) $(DESTDIR)/var/lib/udge/submissions

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
	[ -z "$(HTTPD_USER)" ] || chown $(HTTPD_USER) $(DESTDIR)/var/lib/udge/users
	[ -z "$(HTTPD_USER)" ] || chown $(HTTPD_USER) $(DESTDIR)/var/lib/udge/submissions

start-services:
	systemctl start fcgiwrap.socket
	systemctl start fcgiwrap
	systemctl start nginx

stop-services:
	systemctl stop nginx
	systemctl stop fcgiwrap.socket
	systemctl stop fcgiwrap

enable-nginx-udge-site:
	ln -rs $(NGINX_AVAIL)/udge $(NGINX_ENABLED)/udge
	systemctl reload nginx


test-install:
	[ ! -e pkg ]
	make install       DESTDIR=pkg HTTPD_USER=
	make check-install DESTDIR=pkg
	make uninstall     DESTDIR=pkg
	find pkg -type f
	[ "`find pkg -type f | wc -l`" -eq 2 ] # udgerc and nginx conf
	rm -r pkg

test-dev-install:
	[ ! -e pkg ]
	make dev-install        DESTDIR=pkg HTTPD_USER=
	make check-install-test DESTDIR=pkg
	rm -r pkg

# NOTE: this only works on an "empty" tree.
# Do not use this target to check a real install.
check-install: check-install-test check-install-find

check-install-test:
	diff -rud lib     $(DESTDIR)$(PREFIX)/lib
	diff -rud bin     $(DESTDIR)$(PREFIX)/bin
	diff -rud cgi-bin $(DESTDIR)$(PREFIX)/cgi-bin
	diff -rud etc/udgerc $(DESTDIR)/etc/udgerc
	diff -rud $(NGINX_AVAIL)/udge $(NGINX_ENABLED)/udge
	[ -d $(DESTDIR)/var/lib/udge             ]

check-install-find:
	find pkg -type f | sed -e "s,$(DESTDIR),,;s,$(PREFIX),,;s,^/,," | sort > installed-files.txt
	find lib bin cgi-bin etc -type f                                | sort > installable-files.txt
	diff -rud install{able,ed}-files.txt
	rm install{able,ed}-files.txt
