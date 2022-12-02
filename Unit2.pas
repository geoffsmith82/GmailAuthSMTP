unit Unit2;

interface

uses
    Winapi.Windows
  , Winapi.Messages
  , System.SysUtils
  , System.Variants
  , System.Classes
  , Vcl.Graphics
  , Vcl.Controls
  , Vcl.StdCtrls
  , Vcl.Forms
  , Vcl.Dialogs
  , Vcl.ExtCtrls
  , IdSASL
  , IdSASLCollection
  , IdExplicitTLSClientServerBase
  , EmailOAuthDm
  , IdSASL.Oauth.OAuth2Bearer
  , IdSASL.Oauth.XOAUTH2
  , Email.Demo.Types
  , Globals
  ;

type
  TForm2 = class(TForm)
    btnAuthenticate: TButton;
    btnSendMsg: TButton;
    rgEmailProviders: TRadioGroup;
    btnCheckMsg: TButton;
    btnClearAuthToken: TButton;
    btnCheckIMAP: TButton;
    Memo1: TMemo;
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnCheckMsgClick(Sender: TObject);
    procedure btnClearAuthTokenClick(Sender: TObject);
    procedure rgEmailProvidersClick(Sender: TObject);
    procedure btnCheckIMAPClick(Sender: TObject);
    procedure btnAuthenticateClick(Sender: TObject);
    procedure btnSendMsgClick(Sender: TObject);
  private
    { Private declarations }
    EmailOAuthDataModule : TEmailOAuthDataModule;
    procedure LogMsg(const msg: string);
  public
    { Public declarations }
    procedure UpdateButtonsEnabled;
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

const
  Providers : array[0..2] of TProviderInfo =
  (
    (  AuthenticationType : TIdSASLXOAuth;
       AuthorizationEndpoint : 'https://accounts.google.com/o/oauth2/auth?access_type=offline';
       AccessTokenEndpoint : 'https://accounts.google.com/o/oauth2/token';
       LogoutEndpoint : 'https://www.google.com/accounts/Logout';
       ClientID : google_clientid;
       ClientSecret : google_clientsecret;
       ClientAccount : google_clientAccount;  // your @gmail.com email address
       Scopes : 'https://mail.google.com/ openid';
       SmtpHost : 'smtp.gmail.com';
       SmtpPort : 465;
       PopHost : 'pop.gmail.com';
       PopPort : 995;
       ImapHost : 'imap.gmail.com';
       ImapPort : 143;
       AuthName : 'Google';
       TLS : utUseImplicitTLS;
       TwoLinePOPFormat: False
    ),
    (  AuthenticationType : TIdSASLXOAuth;
       AuthorizationEndpoint : 'https://login.microsoftonline.com/common/oauth2/v2.0/authorize';//'https://login.live.com/oauth20_authorize.srf';
       AccessTokenEndpoint : 'https://login.microsoftonline.com/common/oauth2/v2.0/token';//'https://login.live.com/oauth20_token.srf';
       LogoutEndpoint : 'https://login.microsoftonline.net/common/oauth2/v2.0/logout';
       ClientID : microsoft_clientid;
       ClientSecret : '';
       ClientAccount : microsoftoffice_clientaccount; // your @live.com or @hotmail.com email address
       Scopes : 'https://outlook.office.com/IMAP.AccessAsUser.All https://outlook.office.com/POP.AccessAsUser.All https://outlook.office.com/SMTP.Send offline_access';
       //'wl.imap offline_access';
       SmtpHost : 'smtp-mail.outlook.com';
       SmtpPort : 587;
       PopHost : 'smtp-mail.outlook.com';
       PopPort : 995;
       ImapHost : 'outlook.office365.com';
       ImapPort : 993;
       AuthName : 'Microsoft';
       TLS : utUseExplicitTLS;
       TwoLinePOPFormat: True
    ),
    (  AuthenticationType : TIdSASLXOAuth;
       AuthorizationEndpoint : 'https://login.live.com/oauth20_authorize.srf';
       AccessTokenEndpoint : 'https://login.live.com/oauth20_token.srf';
       LogoutEndpoint : 'https://login.live.com/logout.srf';
       ClientID : microsoft_clientid;
       ClientSecret : '';
       ClientAccount : microsoft_clientAccount; // your @live.com or @hotmail.com email address
      // Scopes : 'https://outlook.office.com/IMAP.AccessAsUser.All https://outlook.office.com/POP.AccessAsUser.All https://outlook.office.com/SMTP.Send offline_access';
       Scopes : 'wl.imap wl.emails wl.offline_access';
       SmtpHost : 'smtp.office365.com';
       SmtpPort : 587;
       PopHost : 'outlook.office365.com';
       PopPort : 995;
       ImapHost : 'outlook.office365.com';
       ImapPort : 993;
       AuthName : 'Hotmail';
       TLS : utUseExplicitTLS;
       TwoLinePOPFormat: false
    )
  );

procedure TForm2.FormDestroy(Sender: TObject);
begin
  FreeAndNil(EmailOAuthDataModule);
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  EmailOAuthDataModule := TEmailOAuthDataModule.Create(nil);
  EmailOAuthDataModule.OnLog := LogMsg;
  EmailOAuthDataModule.Provider := Providers[rgEmailProviders.ItemIndex];
  EmailOAuthDataModule.SetupAuthenticator;
  UpdateButtonsEnabled;
end;


procedure TForm2.UpdateButtonsEnabled;
begin
  btnAuthenticate.Enabled :=  not EmailOAuthDataModule.HasRefreshToken;
  btnClearAuthToken.Enabled :=  EmailOAuthDataModule.HasRefreshToken;
end;

procedure TForm2.btnAuthenticateClick(Sender: TObject);
begin
  EmailOAuthDataModule.Authenticate;
  UpdateButtonsEnabled;
end;

procedure TForm2.btnCheckIMAPClick(Sender: TObject);
begin
  EmailOAuthDataModule.CheckIMAP;
end;

procedure TForm2.btnCheckMsgClick(Sender: TObject);
begin
  EmailOAuthDataModule.CheckPOP;
end;

procedure TForm2.btnClearAuthTokenClick(Sender: TObject);
begin
  EmailOAuthDataModule.ClearAuthentication;
  UpdateButtonsEnabled;
end;

procedure TForm2.btnSendMsgClick(Sender: TObject);
begin
  EmailOAuthDataModule.SendMessage('');
end;

procedure TForm2.LogMsg(const msg: string);
begin
  Memo1.Lines.Add(msg);
end;

procedure TForm2.rgEmailProvidersClick(Sender: TObject);
begin
  EmailOAuthDataModule.SelectedProvider := rgEmailProviders.ItemIndex;
  EmailOAuthDataModule.Provider := Providers[rgEmailProviders.ItemIndex];
  EmailOAuthDataModule.SetupAuthenticator;
  UpdateButtonsEnabled;
end;

end.
