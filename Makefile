# Makefile for the judge

TIDY = tidy -qe --show-filename yes
PREFIX = /usr/local
PUBLIC_HTML = public_html
HTMLS = \
	$(PUBLIC_HTML)/bootstrap.min.css \
	$(PUBLIC_HTML)/404.html \
	$(PUBLIC_HTML)/hello.html \
	$(PUBLIC_HTML)/hello-world.html \
	$(PUBLIC_HTML)/add.html \
	$(PUBLIC_HTML)/index.html
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

clean: clean-html

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

test-makefile: test-makefile-coverage test-link-install

test-makefile-coverage:
	rm -f /tmp/udge-clitests /tmp/udge-txts
	cat Makefile  | grep -E '[-a-z0-9]+\.clitest' | sed -e 's/ *//g; s/.clitest.*//' | sort > /tmp/udge-clitests
	ls -1 examples/*.txt | sed -e 's,examples/,,; s,.txt,,'                          | sort > /tmp/udge-txts
	diff -rud /tmp/udge-clitests /tmp/udge-txts
	rm -f /tmp/udge-clitests /tmp/udge-txts

.PHONY: %.clitest
%.clitest: examples/%.txt
	PATH="./bin:$$PATH" clitest -1 $<

start-services:
	sudo systemctl start fcgiwrap.socket
	sudo systemctl start fcgiwrap
	sudo systemctl start nginx

stop-services:
	sudo systemctl stop nginx
	sudo systemctl stop fcgiwrap.socket
	sudo systemctl stop fcgiwrap

html: $(HTMLS)
	./bin/udge-update-all-users-html
	./bin/udge-update-rank-html

html-force: $(HTMLS)
	./bin/udge-update-all-users-html force
	./bin/udge-update-rank-html

clean-html:
	rm -f $(HTMLS)

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

clean-test-users:
	rm -rf /var/lib/udge/users/test-*-*-*
	rm -rf /var/lib/udge/results/test-*-*-*
	rm -rf /var/lib/udge/submissions/test-*-*-*
	rm -rf /var/lib/udge/html/u/test-*-*-*.html

$(PUBLIC_HTML)/%.css: lib/udge/%.css
	cp $< $@

$(PUBLIC_HTML)/%.html: problem/%/desc lib/udge/markdown lib/udge/html
	mkdir -p $(PUBLIC_HTML)
	./lib/udge/markdown $< > $@

$(PUBLIC_HTML)/%.html: problem/%.md lib/udge/markdown lib/udge/html
	mkdir -p $(PUBLIC_HTML)
	./lib/udge/markdown $< > $@

install:
	mkdir -p                      $(DESTDIR)/etc
	mkdir -p                      $(DESTDIR)/etc/nginx/srv/avail
	mkdir -p                      $(DESTDIR)/var/lib
	mkdir -p                      $(DESTDIR)$(PREFIX)/lib
	mkdir -p                      $(DESTDIR)$(PREFIX)/bin
	mkdir -p                      $(DESTDIR)$(PREFIX)/cgi-bin
	install -m 644 etc/udgerc     $(DESTDIR)/etc/udgerc
	install -m 644 etc/nginx/srv/avail/udge $(DESTDIR)/etc/nginx/srv/avail/udge
	install -m 755 -d             $(DESTDIR)/var/lib/udge
	install -m 755 $(BINS)        $(DESTDIR)$(PREFIX)/bin
	install -m 755 $(CGIBINS)     $(DESTDIR)$(PREFIX)/cgi-bin
	install -m 755 -d             $(DESTDIR)$(PREFIX)/lib/udge
	install -m 644 $(LIBS)        $(DESTDIR)$(PREFIX)/lib/udge
	install -m 755 $(LIBBINS)     $(DESTDIR)$(PREFIX)/lib/udge
	install -m 755 -d             $(DESTDIR)$(PREFIX)/lib/udge/compile
	install -m 755 $(COMPILE)     $(DESTDIR)$(PREFIX)/lib/udge/compile
	install -m 755 -d             $(DESTDIR)$(PREFIX)/lib/udge/compile-as-lib
	install -m 755 $(COMPILELIB)  $(DESTDIR)$(PREFIX)/lib/udge/compile-as-lib
	install -m 755 -d             $(DESTDIR)$(PREFIX)/lib/udge/score
	install -m 755 $(SCORE)       $(DESTDIR)$(PREFIX)/lib/udge/score
	[ "$$EUID" -ne 0 ] || id -u udge >/dev/null 2>&1 || useradd -r -d/var/lib/udge -s/usr/bin/nologin udge

# NOTE: Only use this to set up a development environment, never in a real
#       installation.
#
# This target will fail if your path has spaces.  (-:
link-install:
	mkdir -p                               $(DESTDIR)/etc
	mkdir -p                               $(DESTDIR)/etc/nginx/srv/avail
	mkdir -p                               $(DESTDIR)/var/lib
	ln -sfT `pwd`/etc/udgerc               $(DESTDIR)/etc/udgerc
	ln -sfT `pwd`/etc/nginx/srv/avail/udge $(DESTDIR)/etc/nginx/srv/avail/udge
	install -m 775 -d                      $(DESTDIR)/var/lib/udge
	ln -sfT `pwd`/problem                  $(DESTDIR)/var/lib/udge/problem
	ln -sfT `pwd`/public_html              $(DESTDIR)/var/lib/udge/html
	for dir in `find bin lib cgi-bin -type d`; do \
		mkdir -p $(DESTDIR)$(PREFIX)/$$dir; done
	for file in `find bin lib cgi-bin -type f`; do \
		ln -sf `pwd`/$$file $(DESTDIR)$(PREFIX)/$$file; done

# Use with care.  If there are files installed by other packages but with the
# same name, those will be deleted.
uninstall:
	for file in `find bin lib cgi-bin -type f`; do \
		rm -f $(DESTDIR)$(PREFIX)/$$file; done
	rm -rf $(DESTDIR)$(PREFIX)/lib/udge

# Use with This will move:
#
# * all configuration files
# * all installed problems
# * all submissions
# * all results
today=$(shell date "+%Y%m%d")
purge:
	mv $(DESTDIR)/etc/udgerc{,-old-$(today)}
	mv $(DESTDIR)/etc/nginx/srv/avail/udge{,-old-$(today)}
	mv $(DESTDIR)/var/lib/udge{,-old-$(today)}
	userdel udge

test-link-install:
	[ ! -e pkg ]
	make link-install       DESTDIR=pkg
	make check-install-test DESTDIR=pkg
	rm -r pkg

test-install:
	[ ! -e pkg ]
	make install       DESTDIR=pkg
	make check-install DESTDIR=pkg
	make uninstall     DESTDIR=pkg
	find pkg -type f
	[ "`find pkg -type f | wc -l`" -eq 2 ] # udgerc and nginx conf
	rm -r pkg

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
