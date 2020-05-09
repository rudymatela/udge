Udge
====

Udge is an online judge of programming problems.

Udge is implemented as a collection of (usually bash) scripts.

TODO: example problem and solution

A difference from other judges is that here we allow for problems with partial
scoring, where in other judges the solution has to be perfect, here we give
partial scores for:

* a slower solution ("higher" time complexity)
* a solution that works only on smaller test cases

This is achieved both by permitting multiple sets of input/output files and
custom test scripts.

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
* * * * * /usr/local/bin/udge-update-all-users-html
*/2 * * * * /usr/local/bin/udge-update-rank-html
```

The above will:
`pick-and-judge` and `udge-update-all-users-html` every minute;
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

Except for the main configuration file location, all others are configurable.
Here are sensible defaults for the far-future:

* `/etc/udgerc`:               main configuration file
* `/var/lib/udge/users`:       directory with user information (credentials)
* `/var/lib/udge/problem`:     I/O test cases and markdown files
* `/var/lib/udge/submissions`: submissions that are still to be judged
* `/var/lib/udge/results`:     results of judging submissions
* `/var/lib/udge/html`:        the static HTML pages (problems and users)


### User directory

The user directory stores main user information and credentials.

* `cgi-bin/new-user` creates entries in this directory.
* `cgi-bin/submit` checks credentials in this directory.

User information is stored in plain files under the `users` directory.
Each user is described as a directory with it's name which should be composed
only of English lowercase letters, dashes (`-`) and underscores (`_`).

For now, we store the email and password,
each in its own file with a single line:

```
users/<user>/email
users/<user>/password
```

For example:

```
users/beltrano/email:beltrano@example.com
users/beltrano/password:8165d72190bfc5a0f59adc3bde9b57aced5723656a0e2af21e2a51b03199fbef
users/cicrano/email:cicrano@example.com
users/cicrano/password:158a1d17ff2b5598c2066735f38883a4ba59310ad9dfe74c1ac3f178a7808700
users/fulano/email:fulano@example.com
users/fulano/password:34aceeb563fa984ddad7e549228cdcc88c97a60659d4da7d206192da8997606c
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

This directory may also contain example solutions:

* `tle.c`: example solution in C that yields "Time Limit Exceeded".
* `re.c`: example solution in C that yields "Runtime Error".
* `yes.c`: example solution in C that yields a full score.
* `yes-4-6.hs`: example solution in Haskell that yields 4/6.

If there is only one test set, you are allowed to let `in` and `out` reside
plainly without a subdir.


### Submissions directory

The submissions directory contains submissions that are yet to be scored.

* `cgi-bin/submit` creates entries in this directory
* `bin/udge-pick-and-judge` picks-then-deletes entries from this directory

It contain files in the following format:

* `submissions/<user>/YYYYMMDD-HHMMSS/<problem>.<language>`

For example:

* `submissions/fulano/20200224-202249/add.c`
* `submissions/cicrano/20200224-001234/prod3.hs`


### Results

The results directory contains the results of evaluated solutions.

* `bin/udge-pick-and-judge` creates entries in this directory
* `bin/generate-user-pages` reads from this directory (TBA)

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


Programs
--------

* `cgi-bin/new-user`: handles user creation
* `cgi-bin/submit`: called when an user submits a solution,
					creates a submission in the submissions folder.
* `bin/udge-judge`: judges a solution
* `bin/udge-pick-and-judge`: to be called every 1 minute from cron,
                        picks a solution at random from `submissions`,
						creates a result on `results`.

Others needed:

* a program that reads results and create user pages.  (to be called right after pick-and-judge)
* a program that creates a user ranking  (to be called every 10 minutes or every hour)


Future features
---------------

* User can configure:
	- appear on raking;
	- anonymized on ranking (Anonymous) and private user page (as a single option).
