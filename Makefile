# Makefile for the judge

PUBLIC_HTML=public_html
HTMLS=\
	$(PUBLIC_HTML)/sum2/index.html \
	$(PUBLIC_HTML)/hello/index.html \
	$(PUBLIC_HTML)/index.html

.PHONY: all
all:

clean: clean-html

test: \
  test-happy \
  new-user.clitest \
  submit.clitest \
  sum2.clitest \
  index.clitest

test-happy: \
  happy-day-1.clitest \
  happy-day-2.clitest \
  happy-day-3.clitest

.PHONY: %.clitest
%.clitest: examples/%.txt
	clitest $<

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

$(PUBLIC_HTML)/%/index.html: problem/%/desc
	mkdir -p $(PUBLIC_HTML)/$*
	./bin/markdown $< > $@

$(PUBLIC_HTML)/index.html: problem/index.md
	mkdir -p $(PUBLIC_HTML)
	./bin/markdown $< > $@
