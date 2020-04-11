# Makefile for the judge

TIDY=tidy -qe --show-filename yes
PREFIX=/usr/local
PUBLIC_HTML=public_html
HTMLS=\
	$(PUBLIC_HTML)/404.html \
	$(PUBLIC_HTML)/hello.html \
	$(PUBLIC_HTML)/hello-world.html \
	$(PUBLIC_HTML)/add.html \
	$(PUBLIC_HTML)/index.html

.PHONY: all
all:

clean: clean-html

test: \
  tidy \
  test-scripts \
  test-web

test-scripts: \
  judge.clitest \
  hello-world.clitest \
  hello-world-hs.clitest \
  hello-world-py.clitest \
  add.clitest \
  add-hs.clitest \
  add-py.clitest \
  hello.clitest

test-web: \
  index.clitest \
  new-user.clitest \
  submit.clitest \
  test-happy

test-happy: \
  happy-day-1.clitest \
  happy-day-2.clitest \
  happy-day-3.clitest

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
	rm -rf /etc/udge/users/test-*-*-*
	rm -rf public_html/u/test-*-*-*.html

$(PUBLIC_HTML)/%.html: problem/%/desc lib/udge/markdown lib/udge/html
	mkdir -p $(PUBLIC_HTML)
	./lib/udge/markdown $< > $@

$(PUBLIC_HTML)/%.html: problem/%.md lib/udge/markdown lib/udge/html
	mkdir -p $(PUBLIC_HTML)
	./lib/udge/markdown $< > $@

# NOTE: Only use this to set up a development environment, never in a real
#       installation.
#
# This target will fail if your path has spaces.  (-:
link-install:
	mkdir -p                    /etc/udge
	mkdir -p                    /etc/udge/users
	ln -sfT `pwd`/etc/udge/conf /etc/udge/conf
	ln -sfT `pwd`/etc/udge/salt /etc/udge/salt
	ln -sfT `pwd`/problem       /etc/udge/problem
	ln -sfT `pwd`/public_html   /srv/udge
	for dir in `find bin/ lib/ cgi-bin/ -type d`; do \
		mkdir -p $(PREFIX)/$$dir; done
	for file in `find bin/ lib/ cgi-bin/ -type f`; do \
		ln -sf `pwd`/$$file $(PREFIX)/$$file; done

uninstall:
	for file in `find bin/ lib/ cgi-bin/ -type f`; do \
		rm -f $(PREFIX)/$$file; done
	rm -rf $(PREFIX)/lib/udge
