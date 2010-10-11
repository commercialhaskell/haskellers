This site runs on a RESTful API. Content is available as JSON. In order to access this, you **must** set an HTTP request header of "Accept: application/json". The following resource patterns are available:

## http://www.haskellers.com/users/

FIXME: This API is in flux right now, my appologies

This returns a list of all publicly-viewable user accounts. The response is a JSON map with one attribute: users. users is a JSON list, each element an array with three elements: id is the numerical ID of the account, name is the user's full name and url is the user's Haskeller URL (the next resource pattern).

## http://www.haskellers.com/user/*id*

This contains detailed information on a specific user. It returns a map with the following elements (note: more will be added over time):

* id: numerical ID
* name: user's full name
* website (optional): the user's specified website
* haskell-since (optional): first year user used Haskell
* description (optional): the user's self description
* skills: a JSON list of all skills the user claims

## http://www.haskellers.com/user/

This is for looking up a Haskell account by identifier. This is useful if you have an OpenID-enabled site and you would like to get information on a user. You must provide the user identifier as a query string parameter named "ident". If no account is associated with that identifier, this resource will return a 404. Otherwise, it will return a JSON map with two elements: id and url.

## http://www.haskellers.com/skills/

This returns a list of all skills available on Haskellers. It returns a map with one key: skills. The value is a list of maps with the fields:

* id: The nuemrical ID of the skill
* name: The name of the skill
* url: The resource for this skill (see next entry).

## http://www.haskellers.com/skills/*id*/

Returns a list of users with a given skill. It returns a map with one key: users. The value is a list of maps with the fields:

* id: The user's numerical ID
* name: The user's full name
* url: The resource for this user (see second entry in this file).

# Sample session

    > curl -H "Accept: application/json" http://www.haskellers.com/

    {"users":[{"id":"5","name":"Michael Snoyman","url":"http://www.haskellers.com/user/5/"}]}

    > curl -H "Accept: application/json" http://www.haskellers.com/user/5/

    {"id":"5","name":"Michael Snoyman","website":"http://www.snoyman.com/","experience":"3","description":"This is a test description.\r\n\r\nvery very very very very very very very very very very very very very very very very very very very very long line\r\n\r\nThis is <only a &amp; test>","skills":["Web development","Parsec"]}

    > curl -H "Accept: application/json" http://www.haskellers.com/user/?ident=http://snoyberg.wordpress.com/

    {"id":"5","url":"http://www.haskellers.com/user/5/"}
