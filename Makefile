# Makefile for the judge

.PHONY: all
all:

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

html: \
	public_html/sum2/index.html \
	public_html/hello/index.html

public_html/%/index.html: problem/%/desc
	mkdir -p public_html/$*
	# TODO: ./bin/markdown that also adds a header and footer
	markdown $< > $@
