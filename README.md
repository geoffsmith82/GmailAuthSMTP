# GmailAuthSMTP
This project is a very basic demo showing how to authenticate with OAUTH2 and send an email message.

You will need to create a file called GmailGlobals.pas which contains the following constants

* clientid
* clientsecret
* clientscope
* clientredirect
* clientaccount
* clientsendtoaddress
* clientname

With some further work, this could be further improved to remove some of the required constants and improving some of the error handling.

With the correct scopes, the authentication process could also return a refresh_token that could be saved so that you would not need to login/authorize everytime you start the program.



Thanks

Geoffrey Smith
