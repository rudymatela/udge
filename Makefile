# Makefile for the judge

TIDY = tidy -qe --show-filename yes
PREFIX = /usr/local
HTTPD_USER = http
BINS = \
	bin/cgi-create-data-files \
	bin/udge-add-user \
	bin/udge-judge \
	bin/udge-latest-results \
	bin/udge-pick-and-judge \
	bin/udge-rank \
	bin/udge-sandbox \
	bin/udge-submit \
	bin/udge-update-all-problem-htmls \
	bin/udge-update-all-users-html \
	bin/udge-update-rank-html \
	bin/udge-update-user-html \
	bin/udge-user-stats
CGIBINS = \
	cgi-bin/udge-new-user \
	cgi-bin/udge-submit
LIBS = \
	lib/udge/bootstrap.min.css \
	lib/udge/html \
	lib/udge/cgi \
	lib/udge/core
LIBBINS = \
	lib/udge/markdown \
	lib/udge/rank-html \
	lib/udge/user-html \
	lib/udge/compile-and-test
COMPILE = \
	lib/udge/compile/c \
	lib/udge/compile/hs \
	lib/udge/compile/py
COMPILELIB = \
	lib/udge/compile-as-lib/c \
	lib/udge/compile-as-lib/hs \
	lib/udge/compile-as-lib/py
SCORE = \
	lib/udge/score/fractions \
	lib/udge/score/icpc \
	lib/udge/score/solved \
	lib/udge/score/sum

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

test-makefile: test-makefile-coverage test-dev-install

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
	./bin/udge-update-all-users-html
	./bin/udge-update-rank-html

html-force:
	./bin/udge-update-all-problem-htmls force
	./bin/udge-update-all-users-html force
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
	mkdir -p                      $(DESTDIR)/etc
	mkdir -p                      $(DESTDIR)/etc/nginx/srv/avail
	mkdir -p                      $(DESTDIR)/var/lib
	mkdir -p                      $(DESTDIR)$(PREFIX)/lib
	mkdir -p                      $(DESTDIR)$(PREFIX)/bin
	mkdir -p                      $(DESTDIR)$(PREFIX)/cgi-bin
	install -m 0644 etc/udgerc     $(DESTDIR)/etc/udgerc
	install -m 0644 etc/nginx/srv/avail/udge $(DESTDIR)/etc/nginx/srv/avail/udge
	install -m 0755 -d             $(DESTDIR)/var/lib/udge
	install -m 2770 -d            $(DESTDIR)/var/lib/udge/users
	install -m 2775 -d            $(DESTDIR)/var/lib/udge/submissions
	install -m 0755 $(BINS)        $(DESTDIR)$(PREFIX)/bin
	install -m 0755 $(CGIBINS)     $(DESTDIR)$(PREFIX)/cgi-bin
	install -m 0755 -d             $(DESTDIR)$(PREFIX)/lib/udge
	install -m 0644 $(LIBS)        $(DESTDIR)$(PREFIX)/lib/udge
	install -m 0755 $(LIBBINS)     $(DESTDIR)$(PREFIX)/lib/udge
	install -m 0755 -d             $(DESTDIR)$(PREFIX)/lib/udge/compile
	install -m 0755 $(COMPILE)     $(DESTDIR)$(PREFIX)/lib/udge/compile
	install -m 0755 -d             $(DESTDIR)$(PREFIX)/lib/udge/compile-as-lib
	install -m 0755 $(COMPILELIB)  $(DESTDIR)$(PREFIX)/lib/udge/compile-as-lib
	install -m 0755 -d             $(DESTDIR)$(PREFIX)/lib/udge/score
	install -m 0755 $(SCORE)       $(DESTDIR)$(PREFIX)/lib/udge/score
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
	mv $(DESTDIR)/etc/nginx/srv/avail/udge{,-old-$(now)}
	mv $(DESTDIR)/var/lib/udge{,-old-$(now)}
	userdel udge

test-dev-install:
	[ ! -e pkg ]
	make dev-install        DESTDIR=pkg HTTPD_USER=
	make check-install-test DESTDIR=pkg
	rm -r pkg

test-install:
	[ ! -e pkg ]
	make install       DESTDIR=pkg HTTPD_USER=
	make check-install DESTDIR=pkg
	make uninstall     DESTDIR=pkg
	find pkg -type f
	[ "`find pkg -type f | wc -l`" -eq 2 ] # udgerc and nginx conf
	rm -r pkg

# Run this as your regular user before dev-install
dev-setup:
	install -m 0755 -d var
	install -m 2770 -d var/users
	install -m 2775 -d var/submissions
	ln -rsfT problem var/problem

# Run this as root after dev-setup
dev-install:
	mkdir -p                               $(DESTDIR)/etc
	mkdir -p                               $(DESTDIR)/etc/nginx/srv/avail
	mkdir -p                               $(DESTDIR)/var/lib
	ln -sfT `pwd`/etc/udgerc               $(DESTDIR)/etc/udgerc
	ln -sfT `pwd`/etc/nginx/srv/avail/udge $(DESTDIR)/etc/nginx/srv/avail/udge
	mkdir -p var
	ln -sfT `pwd`/var                      $(DESTDIR)/var/lib/udge
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

enable-udge-site:
	ln -rs /etc/nginx/srv/{avail,enabled}/udge
	systemctl reload nginx

# NOTE: this only works on an "empty" tree.
# Do not use this target to check a real install.
check-install: check-install-test check-install-find

check-install-test:
	diff -rud lib     $(DESTDIR)$(PREFIX)/lib
	diff -rud bin     $(DESTDIR)$(PREFIX)/bin
	diff -rud cgi-bin $(DESTDIR)$(PREFIX)/cgi-bin
	diff -rud etc/udgerc $(DESTDIR)/etc/udgerc
	[ -d $(DESTDIR)/var/lib/udge             ]

check-install-find:
	find pkg -type f | sed -e "s,$(DESTDIR),,;s,$(PREFIX),,;s,^/,," | sort > installed-files.txt
	find lib bin cgi-bin etc -type f                                | sort > installable-files.txt
	diff -rud install{able,ed}-files.txt
	rm install{able,ed}-files.txt
