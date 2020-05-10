Udge
====

Udge is an online judge for programming problems.

TODO: example problem and solution

Using Udge you can host programming problems in an HTTP site.
Users can submit solution programs which are automatically scored.
Scores are then reported in each user's page.

Uses:

* hosting programming contests;
* hosting programming problems for students by teachers, lecturers or professors;
* hosting an online judge.

The usual goal of problems hosted on Udge is to create a command line program
that reads data from standard input and produces results on standard output.

Udge is not a collection of programming problems
but rather the software used to host them.
You should create your own problems:
write the problem description in markdown
and create input and output cases
following [Udge's Problem directory] format.
Udge comes with four example problems illustrating this.

[Udge's Problem directory]: #problem-directory

Features:

* multiple sets of input/output files per problem -- graded scores;
* problem description in markdown;
* support for "library" solutions
  where one has to implement a specific function;
* a rank with a few selectable formats;
* simple plaintext file database, no need to setup an SQL server and database;
* (for now) support for solutions in C, Python and Haskell.

Udge is implemented in Bash and works on Linux systems with Nginx.
It uses static HTML pages where possible.
These pages are updated by minutely cron jobs.
The only two dynamic pages are for submission and user creation.

Udge is free/libre software.
It is available under the GPLv2 license
unless otherwise stated in specific files.
--- Copyright (C) 2015-2020  Rudy Matela ---
This program is distributed in the hope that it will be useful,
but __without any warranty__; without even the implied warranty of
__merchantability__ or __fitness for a particular purpose__.
See the GNU General Public License for more details.

Udge is already functional and usable but it is a _work in progress_.
Submission sandboxing is still pretty limited so use with care.
For now, only give submission access to people you trust:
either deactivate user creation or run this only on a local network.


Dependencies
------------

To install and run Udge, you will need:

* Bash
* Python
* nginx
* fcgiwrap
* fakechroot
* util-linux --- for sandboxing with `unshare`
* discount   --- for `markdown`
* cronie
* GCC's GCC  --- for C submission support
* GHC        --- for Haskell submission support
* clitest    --- for testing Udge itself
* tidy       --- for testing Udge itself
* TODO: complete this list

[dependencies]: #dependencies


Installing and Configuring
--------------------------

TODO: provide packages for Arch Linux and Ubuntu.

First make sure you have all the [dependencies] installed.  Then:

1. run `make install` as `root`:

		$ sudo make install

	Depending on your Linux distribution, you may need to set `HTTPD_USER`,
	`NGINX_AVAIL` and `NGINX_ENABLED`:

		$ sudo make install HTTPD_USER=<user> NGINX_AVAIL=<path> NXING_ENABLED=<path>

	The Makefile should be able to figure these automatically on Arch Linux
	(tested) and on Debian/Ubuntu variants (untested).

2. (optional)
	add your problems to `/var/lib/udge/problem`
	and update `index.md` accordingly
	otherwise you will be using the default example problems.
	This can be done at a later point.

3. generate static HTML files:

		sudo -u udge udge-update-all-problem-htmls
		sudo -u udge udge-update-rank-html

	You will have to re-run `udge-update-all-problem-htmls` every time you add
	or edit a problem description so HTML files are updated.

4. set the following on `udge` user's crontab with `sudo -u udge crontab -e`

		* * * * * /usr/local/bin/udge-pick-and-judge
		* * * * * /usr/local/bin/udge-update-all-user-htmls
		*/2 * * * * /usr/local/bin/udge-update-rank-html

	The above will:
	`pick-and-judge` and `udge-update-all-user-htmls` every minute;
	`udge-update-rank-html` every 2 minutes.
	Please adapt as needed.

5. Add the following entry to `/etc/hosts`.

		127.0.0.1 udge udge.localdomain

	If you have another domain name you would like to use,
		replace it here instead.
	If you already have a public DNS entry pointing to your server
		you may skip this step.

6. (optional) edit the domain name on Udge's Nginx config, located usually on either:

	- `/etc/nginx/srv/avail/udge`; or
	- `/etc/nginx/srv/sites-available/udge`
	- ...

	the actual location will depend on your Linux distribution.


7. (optional) start the Nginx server if you haven't done so with either:

	- `systemctl start nginx`; or
	- `service nginx start`; or
	- `/etc/inid.d/nginx start`; or
	- ...

	which will depend on your Linux distribution.

8. enable Udge on Nginx and reload the configuration:

		make enable-nginx-udge-site

9. test that everything works
	by typing `udge/` (or your selected domain of steps 5 and 6)
	in your browser's address bar.
	Do not forget the `/` at the end, otherwise your browser may search the web
	for "udge".  If it does not work try also `http://udge/`.

	You should see the problem index and the menu at the top and bottom.

Udge can be customized on it's configuration file `/etc/udgerc`.

[Installing and Configuring]: #installing-and-configuring


Pages and Routes
----------------

If the installation is working you should be able to access the following pages,
each accessed by typing `udge/<page>` or `<yourdomain>/<page>`:

* `/`:              the index with the list of problems (`index.md`)
* `/submit`:        submission of solutions
* `/new-user`:      user creation
* `/<problem>`:     a problem description, e.g.:
	- `/hello`
	- `/add`
	- `/hello-world`
* `/u/<user_name>`: user's page with
                    scores for each problem and latest submissions
* `/rank`:          the user rank


Creating a Problem
------------------

To create a problem on Udge:

1. Create a subdirectory to `/var/lib/udge/problem` called:

		/var/lib/udge/problem/<problem-code>

	The problem code should contain only
		digits,
		lowercase English letters and
		dashes (`-`).

2. Create three files in the newly created problem folder:

	* `desc`: a markdown file with your problem description

	* `in`: the input file.
		This will be used as the standard input to submitted solutions:

			./submitted-program <in

	* `sol`: the solution file.
		This will be compared to the standard output of submitted solutions:

			./submitted-program <in >out
			diff -rud out sol

		If output matches exactly, the submission will get a score.

3. Link to the newly created problem on the problem index
	by editing the following file:

		/var/lib/udge/problem/index.md

4. Update static HTML files by running:

		sudo -u udge udge-update-all-problem-htmls

5. (testing) Access the newly created problem on:

		http://udge/<problem-code>

6. (testing) Submit a _correct_ solution to the newly created problem
	and make sure it receives a full score.

7. (testing) Submit an _incorrect_ solution to the newly created problem
	and make sure it does not receive a full score.

Please see the `addition` problem for an example of this.


### Multiple input and output pairs

To set up a problem with multiple I/O pairs,
instead of creating just `in` and `sol`,
create several subdirectories `1`, `2`, `3`, ...
Inside each create `in` and `out` files:

* `1/in`
* `1/out`
* `2/in`
* `2/out`
* `3/in`
* `3/out`
* ...

Please see the `add` problem for an example of this.



Development Information
-----------------------

This section contains development information to those interested in forking or
contributing to Udge's development.


### Setting up a Development Environment

This section is intended for people who want to work on Udge development
itself.  If you just want to _use_ Udge to host problems please see the
[Installing and Configuring] section instead.

First make sure you have all the [dependencies] installed.
Make sure you _don't_ have Udge installed by `make install`.
Use `make uninstall-and-purge` if needed.

The following sequence of commands can be used to set up the development
environment.  Run them as your _regular user_.  You should only use `root`
while running those preceded by `sudo`.

	make dev-setup
	sudo make dev-install
	make html
	sudo make start-services
	sudo make enable-nginx-udge-site

You should also add `127.0.0.1 udge` to `/etc/hosts`.

If everything worked correctly,
you should be able to run `make test` successfully.

If you like your development environment
to automatically judge and update pages in the background,
you should also add the following to your regular user's crontab
(use `crontab -e`):

	* * * * * /usr/local/bin/udge-pick-and-judge
	* * * * * /usr/local/bin/udge-update-all-user-htmls
	*/2 * * * * /usr/local/bin/udge-update-rank-html


### Programs and Commands

Here's a complete list of programs provided with Udge:

* `cgi-bin/udge-new-user`: CGI script that handles user creation;
* `cgi-bin/udge-submit`: CGI script that handles submission of solution;
* `udge-add-user`: adds a user creating an entry on `users`.
* `udge-judge`: judges a solution printing results to stdout
* `udge-latest-results`: shows the latest results from a user
* `udge-pick-and-judge`: to be called every 1 minute from cron,
                         picks a solution at random from `submissions`,
                         creates a result on `results`.
* `udge-rank`: computes and prints the current user rank
* `udge-sandbox`: runs a program in a sandbox.
	Currently sandboxing has a few limitations:
	forking protection can be improved;
	it does not chroot yet (as the chroot folders are still TBD);
	it is not applied to compilation (but should).
	These should be addressed in the future.
* `udge-submit`: submits a solution to the judge using HTTP
* `udge-update-all-problem-htmls`: updates all problem HTML files
* `udge-update-all-user-htmls`: updates all user HTML files
* `udge-update-rank-html`: updates the rank HTML file
* `udge-update-user-html`: updates a single user HTML file
* `udge-user-stats`: prints the stats for a given user


### A file-based Database

Udge stores information about problems, users, submissions and results
in plain text files:

* `/var/lib/udge/users`:       directory with user information (credentials)
* `/var/lib/udge/problem`:     I/O test cases and markdown files
* `/var/lib/udge/submissions`: submissions that are still to be judged
* `/var/lib/udge/results`:     results of judging submissions
* `/var/lib/udge/html`:        the static HTML pages (problems and users)

This section describes the structure of each of these directories.


#### User Directory --- `/var/lib/udge/users`

The user directory stores main user information and credentials.

* `bin/udge-add-user` creates entries in this directory.
* `cgi-bin/udge-new-user` uses `bin/udge-add-user` to create entries here.
* `cgi-bin/udge-submit` checks credentials in this directory.

User information is stored in plain files under the `users` directory.
Each user is described as a directory with it's name which should be composed
only of English lowercase letters, dashes (`-`) and underscores (`_`).

Emails and passwords are stored each in its own file with a single line:

	/var/lib/udge/users/<user>/email
	/var/lib/udge/users/<user>/password
	/var/lib/udge/users/<user>/salt

For example:

	users/janeroe/salt:aSTR1PRypdeUUPeX7NFZYwVWrlXac4MYZHoCUIaq
	users/janeroe/email:janeroe@example.net
	users/janeroe/password:e0b3400da3f9edc96718a1b5d0da315f518e36b820404635998319662828fe44
	users/johndoe/salt:QHFNE6WhJD9VoRGeLljOGwBZz//LTXUfnzJpw1k9
	users/johndoe/email:johndoe@example.com
	users/johndoe/password:edbe9e7dd28ca60a1874c88f036513bcf0bcc4d8b5d1f7d875e4fc37b8059828


#### Problem Directory --- `/var/lib/udge/problem`

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


#### Submissions Directory --- `/var/lib/udge/submissions`

The submissions directory contains submissions that are yet to be scored.

* `cgi-bin/udge-submit` creates entries in this directory
* `bin/udge-pick-and-judge` picks-then-deletes entries from this directory

It contain files in the following format:

	submissions/<user>/YYYYMMDD-HHMMSS/<problem>.<language>

For example:

	submissions/johndoe/20200224-202249/add.c
	submissions/janeroe/20200224-001234/hello.hs


#### Results Directory --- `/var/lib/udge/submissions`

The results directory contains the results of evaluated solutions.

* `bin/udge-pick-and-judge` creates entries in this directory
* `bin/udge-update-all-user-htmls` reads from this directory
* `bin/udge-update-user-html` reads from this directory

Results contain a folder for each user which in turn contains a folder for each
problem which contains the best result for a problem along with a folder for
each of the submissions, like so:

	results/<user>/<problem>/best
	results/<user>/<problem>/YYYYMMDD-HHMMSS/result
	results/<user>/<problem>/YYYYMMDD-HHMMSS/<problem>.<language>

For example:

	results/fulano/hello/best
	results/fulano/hello/20190101-133700/result
	results/fulano/hello/20190101-133700/hello.py


### Test & Examples directory

The `examples/` directory contains examples of use for several programs and
commands shipped with Udge.  These double as tests using the `clitest` tool.

Example solutions to example problems are also stored in this directory
under `examples/<problem>`:

* `examples/add/0-ce.c`:     example solution to `add`   in C      -- 0/6 -- compile error;
* `examples/add/0-re.c`:     example solution to `add`   in C      -- 0/6 -- runtime error;
* `examples/hello/hello.c`:  example solution to `hello` in C      -- 6/6;
* `examples/add/add.py`:     example solution to `add`   in Python -- 6/6;
* `examples/add/4-octals.c`: example solution to `add`   in C      -- 4/6 -- wrong output
