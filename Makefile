# Makefile for the judge

PUBLIC_HTML=public_html
HTMLS=\
	$(PUBLIC_HTML)/hello.html \
	$(PUBLIC_HTML)/hello-world.html \
	$(PUBLIC_HTML)/add.html \
	$(PUBLIC_HTML)/index.html

.PHONY: all
all:

clean: clean-html

test: \
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
	clitest -1 $<

start-services:
	sudo systemctl start fcgiwrap.socket
	sudo systemctl start fcgiwrap
	sudo systemctl start nginx

stop-services:
	sudo systemctl stop nginx
	sudo systemctl stop fcgiwrap.socket
	sudo systemctl stop fcgiwrap

html: $(HTMLS)

clean-html:
	rm -f $(HTMLS)

clean-test-users:
	rm -rf /etc/udge/users/test-*-*-*

$(PUBLIC_HTML)/%.html: problem/%/desc bin/markdown
	mkdir -p $(PUBLIC_HTML)
	./bin/markdown $< > $@

$(PUBLIC_HTML)/%.html: problem/%.md bin/markdown
	mkdir -p $(PUBLIC_HTML)
	./bin/markdown $< > $@

diff-nginx:
	diff -rud {,/}etc/nginx/srv/avail/udge
