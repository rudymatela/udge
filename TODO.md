TODO list for Udge
==================

* carry on with the implementation of `udge-health`,
  see `TODO:` comments over there

* remove the requirement of setting most options on `/etc/udgerc`

* support multiple sites by multiple udgerc files (`UDGERC=/etc/blahrc`)

* update the README video

* configure rate limiting on nginx
  https://www.nginx.com/blog/rate-limiting-nginx/


For later
---------

* (help needed) Ubuntu / Debian package

* (help needed) Dockerfile and docker container

* (quasi-stateless) User settings

		udge.example.com/settings
		# User settings
		Username: ______
		Password: ______

		udge.example.com/settings/user
		# User settings for user
		Name: ______
		Email: ______
		Email confirmation: ______
		Password: ______
		To change your username please contact admin@udge.example.com

* (quasi-stateless) Password reset

		udge.example.com/reset-password
		# Reset your password
		username: ______
		email: ______

		udge.example.com/reset-password/username?t=0123456789ABCDEF
		# Reset your password
		new password: ______
		new password confirmation: ______

* cleanup any files created at `tmp` somehow
  `udge-health` needs to be completed first

* Let users configure if they would be appear anonymized on the ranking and
  have a private user page.  (All this as a single option.)
  The private user page would be accessed on `/p/<user_sha1_base64_etc>`

* Add a user's submission page?  `u/<user_name>/submissions`

* Languages to support later (in no particular order):

	- R
	- Perl
	- PHP

* On the user page, only show problems with tries (for now, keep as it is)
