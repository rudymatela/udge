Udge
====

Udge is an online judge of programming problems.

TODO: example problem and solution

Features:

* problems with partial scoring;
* multiple sets of input/output files per problem;
* "library" solutions where one has to implement a specific function;
* (for now) support for solutions in C, Python and Haskell.

Udge is implemented mainly in Bash and works on Linux systems with Nginx.

Udge is free/libre software.
It is available under the GPL license
unless otherwise stated in specific files.


Dependencies
------------

To install and run Udge, you will also need:

* Bash
* Python
* nginx
* fcgiwrap
* fakechroot
* util-linux (for `unshare`)
* discount (for `markdown`)
* clitest
* TODO: complete this list

[dependencies]: #dependencies


Installing and configuring
--------------------------

First make sure you have all the [dependencies] installed.

TBA


Setting up a development environment
------------------------------------

First make sure you have all the [dependencies] installed.

The following sequence of commands can be used to set up the development
environment.  Run them as your _regular user_.  You should only use `root`
while running those preceded by `sudo`.

```
make dev-setup
sudo make dev-install
make html
sudo make start-services
sudo make enable-nginx-udge-site
```

You should also add `127.0.0.1 udge` to `/etc/hosts`.

If everything worked correctly,
you should be able to run `make test` successfully.

If you like your development environment
to automatically run jobs in the background,
you should also add the following to your crontab (use `crontab -e`):

```
* * * * * /usr/local/bin/udge-pick-and-judge
* * * * * /usr/local/bin/udge-update-all-user-htmls
*/2 * * * * /usr/local/bin/udge-update-rank-html
```

The above will:
`pick-and-judge` and `udge-update-all-user-htmls` every minute;
`udge-update-rank-html` every 2 minutes.
Please adapt as needed.

Routes
------

* `/`:              the index with the list of problems
* `/submit`:        allows submission of a solution to a problem
* `/new-user`:      allows creation of a user
* `/<problem>`:     a problem description
	- `/hello`:     the `hello` problem
	- `/add`:       the `add` problem
	- `/prod3`:     the `prod3` problem
* `/u/<user_name>`: user's page, lists scores for each problem (TBA)
* `/p/<user_sha1>`: private user page, lists scores for each problem (TBA)
* `/u/<user_name>/submissions`: lists the submissions of a user    (TBA)
* `/rank`:          the user rank (TBA)


Files (database)
----------------

There are the configuration file and folder locations for Udge:

* `/etc/udgerc`:               main configuration file
* `/var/lib/udge/users`:       directory with user information (credentials)
* `/var/lib/udge/problem`:     I/O test cases and markdown files
* `/var/lib/udge/submissions`: submissions that are still to be judged
* `/var/lib/udge/results`:     results of judging submissions
* `/var/lib/udge/html`:        the static HTML pages (problems and users)


### User directory

The user directory stores main user information and credentials.

* `bin/udge-add-user` creates entries in this directory.
* `cgi-bin/udge-new-user` uses `bin/udge-add-user` to create entries here.
* `cgi-bin/udge-submit` checks credentials in this directory.

User information is stored in plain files under the `users` directory.
Each user is described as a directory with it's name which should be composed
only of English lowercase letters, dashes (`-`) and underscores (`_`).

Emails and passwords are stored each in its own file with a single line:

```
/var/lib/udge/users/<user>/email
/var/lib/udge/users/<user>/password
/var/lib/udge/users/<user>/salt
```

For example:

```
/var/lib/udge/users/janeroe/salt:aSTR1PRypdeUUPeX7NFZYwVWrlXac4MYZHoCUIaq
/var/lib/udge/users/janeroe/email:janeroe@example.net
/var/lib/udge/users/janeroe/password:e0b3400da3f9edc96718a1b5d0da315f518e36b820404635998319662828fe44
/var/lib/udge/users/johndoe/salt:QHFNE6WhJD9VoRGeLljOGwBZz//LTXUfnzJpw1k9
/var/lib/udge/users/johndoe/email:johndoe@example.com
/var/lib/udge/users/johndoe/password:edbe9e7dd28ca60a1874c88f036513bcf0bcc4d8b5d1f7d875e4fc37b8059828
```


### Problem directory

The problem directory contains test scripts, inputs and solutions for each of the problem.

* `bin/udge-judge` reads this directory

Each problem has a directory, `/var/lib/udge/problem/<problem>`.  Inside it:

* `1/in`: test input 1
* `1/sol`: solution for test input 1
* `1/time-limit`: the time limit in seconds (1 if not present)
* `2/...`: test set 2
* `3/...`: test set 3
* ...

If there is only one test set, you are allowed to let `in` and `out` reside
plainly without a subdir.


### Submissions directory

The submissions directory contains submissions that are yet to be scored.

* `cgi-bin/udge-submit` creates entries in this directory
* `bin/udge-pick-and-judge` picks-then-deletes entries from this directory

It contain files in the following format:

* `submissions/<user>/YYYYMMDD-HHMMSS/<problem>.<language>`

For example:

* `submissions/fulano/20200224-202249/add.c`
* `submissions/cicrano/20200224-001234/prod3.hs`


### Results

The results directory contains the results of evaluated solutions.

* `bin/udge-pick-and-judge` creates entries in this directory
* `bin/udge-update-all-user-htmls` reads from this directory
* `bin/udge-update-user-html` reads from this directory

Results contain a folder for each user which in turn contains a folder for each
problem which contains the best result for a problem along with a folder for
each of the submissions, like so:

```
results/<user>/<problem>/best
results/<user>/<problem>/YYYYMMDD-HHMMSS/result
results/<user>/<problem>/YYYYMMDD-HHMMSS/<problem>.<language>
```

For example:

```
results/fulano/hello/best
results/fulano/hello/20190101-133700/result
results/fulano/hello/20190101-133700/hello.py
```


Programs and Commands
---------------------

* `cgi-bin/udge-new-user`: CGI script that handles user creation;
* `cgi-bin/udge-submit`: CGI script that handles submission of solution;
* `udge-add-user`: adds a user creating an entry on `users`.
* `udge-judge`: judges a solution printing results to stdout
* `udge-latest-results`: shows the latest results from a user
* `udge-pick-and-judge`: to be called every 1 minute from cron,
                         picks a solution at random from `submissions`,
                         creates a result on `results`.
* `udge-rank`: computes and prints the current user rank
* `udge-sandbox`: runs a program in a sandbox
* `udge-submit`: submits a solution to the judge using HTTP
* `udge-update-all-problem-htmls`: updates all problem htmls
* `udge-update-all-user-htmls`: updates all user htmls
* `udge-update-rank-html`: updates the rank html
* `udge-update-user-html`: updates a single user html
* `udge-user-stats`: prints the stats for a given user


Test & Examples directory
-------------------------

The `examples/` directory contains examples of use for several programs and
commands shipped with Udge.  These double as tests for the `clitest` tool.

Example solutions to example problems are also stored in this directory
under `examples/<problem>`:

* `examples/add/0-ce.c`:
	example solution to `add` in C
	that yields a compile error with a score of 0/6.
* `examples/add/0-re.c`:
	example solution to `add` in C that
	yields a runtime error with a score of 0/6.
* `examples/hello/hello.c`:
	example solution to `hello` in C
	that gets a full score of 6/6.
* `examples/hello/hello.py`:
	example solution to `hello` in Python
	that gets a full score of 6/6.
* `examples/add/4-octals.c`:
	example solution to `add` in C
	that gets a score of 4/6.
