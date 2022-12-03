# GmailAuthSMTP
This project is a very basic demo showing how to authenticate with OAUTH2 and send an email message for [gmail](https://www.gmail.com), [microsoft/office365](https://outlook.office.com/mail/) as well as [hotmail.com/outlook.com/live.com](https://www.outlook.com) email addresses.
![](Images/SampleIMAPSession.png)
## Google Setup

You will need to create a file called Globals.pas which contains the following constants
![](Images/GooglePermissions.png)
  * google_clientid
  * google_clientsecret
To get these, go to https://console.cloud.google.com/apis/credentials

  * google_clientaccount

## Microsoft Setup

  * microsoft_clientid
  * microsoft_clientaccount
To get these, go to https://portal.azure.com/#blade/Microsoft_AAD_RegisteredApps/ApplicationsListBlade
Microsoft will also need various app permissions as shown below
![](Images/MSPermissions.png)

  * clientsendtoaddress
  * clientname


Thanks

Geoffrey Smith
