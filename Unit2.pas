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
  , Vcl.ComCtrls
  , IdSASL
  , IdSASLCollection
  , IdExplicitTLSClientServerBase
  , EmailOAuthDm
  , IdSASL.Oauth.OAuth2Bearer
  , IdSASL.Oauth.XOAUTH2
  , Email.Demo.Types
  , Globals  // rename from globals.sample.pas and update contents if missing
  ;

type
  TForm2 = class(TForm)
    btnAuthenticate: TButton;
    btnSendMsg: TButton;
    rgEmailProviders: TRadioGroup;
    btnCheckMsg: TButton;
    btnClearAuthToken: TButton;
    btnCheckIMAP: TButton;
    mmoLogging: TMemo;
    btnSendViaREST: TButton;
    PageControl1: TPageControl;
    tsEmail: TTabSheet;
    tsLogging: TTabSheet;
    lblFrom: TLabel;
    lblRecipientAddress: TLabel;
    edtFromAddress: TEdit;
    lblFromName: TLabel;
    edtFromName: TEdit;
    lblRecipientName: TLabel;
    edtRecipientAddress: TEdit;
    edtRecipientName: TEdit;
    mmoBody: TMemo;
    lblSubject: TLabel;
    edtSubject: TEdit;
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnCheckMsgClick(Sender: TObject);
    procedure btnClearAuthTokenClick(Sender: TObject);
    procedure rgEmailProvidersClick(Sender: TObject);
    procedure btnCheckIMAPClick(Sender: TObject);
    procedure btnAuthenticateClick(Sender: TObject);
    procedure btnSendMsgClick(Sender: TObject);
    procedure btnSendViaRESTClick(Sender: TObject);
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
  Providers : array[0..2] of TMailProviderInfo =
  (
    (  AuthenticationType : TIdSASLXOAuth;
       AuthorizationEndpoint : 'https://accounts.google.com/o/oauth2/auth?access_type=offline';
       AccessTokenEndpoint : 'https://accounts.google.com/o/oauth2/token';
       LogoutEndpoint : 'https://www.google.com/accounts/Logout';
       ClientID : google_clientid;
       ClientSecret : google_clientsecret;
       Scopes : 'https://mail.google.com/ openid email';
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
       LogoutEndpoint : 'https://login.microsoftonline.com/common/oauth2/v2.0/logout';
       ClientID : microsoft_clientid;
       ClientSecret : '';
       Scopes : 'https://outlook.office.com/IMAP.AccessAsUser.All https://outlook.office.com/POP.AccessAsUser.All https://outlook.office.com/SMTP.Send offline_access openid email profile';
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
      // Scopes : 'https://outlook.office.com/IMAP.AccessAsUser.All https://outlook.office.com/POP.AccessAsUser.All https://outlook.office.com/SMTP.Send offline_access';
       Scopes : 'wl.imap wl.emails wl.offline_access openid email profile';
       SmtpHost : 'smtp-mail.outlook.com';
       SmtpPort : 587;
       PopHost : 'outlook.office365.com';
       PopPort : 995;
       ImapHost : 'imap-mail.outlook.com';
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
  EmailOAuthDataModule.HWNDHandle := Self.Handle;
  EmailOAuthDataModule.AppHandle := Application.Handle;
  EmailOAuthDataModule.Provider := Providers[rgEmailProviders.ItemIndex];
  EmailOAuthDataModule.SetupAuthenticator;
  edtFromAddress.Text := EmailOAuthDataModule.SendAddress;
  edtFromName.Text := EmailOAuthDataModule.ReadString('FromName', '');
  edtSubject.Text :=  EmailOAuthDataModule.ReadString('Subject', '');
  edtRecipientAddress.Text := EmailOAuthDataModule.ReadString('RecipientAddress', '');
  edtRecipientName.Text := EmailOAuthDataModule.ReadString('RecipientName', '');
  UpdateButtonsEnabled;
end;


procedure TForm2.UpdateButtonsEnabled;
begin
  btnAuthenticate.Enabled :=  not EmailOAuthDataModule.HasRefreshToken;
  btnClearAuthToken.Enabled :=  EmailOAuthDataModule.HasRefreshToken;
  btnSendViaREST.Enabled := rgEmailProviders.ItemIndex = 1;
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
  if string(edtRecipientAddress.Text).IsEmpty then
  begin
    ShowMessage('Recipient Email Address Required');
    Exit;
  end;

  EmailOAuthDataModule.SendMessage(edtFromAddress.Text, edtFromName.Text,
                          edtRecipientAddress.Text, edtRecipientName.Text,
                          edtSubject.Text, mmoBody.Text, '');
  EmailOAuthDataModule.WriteString('FromName', edtFromName.Text);
  EmailOAuthDataModule.WriteString('Subject', edtSubject.Text);
  EmailOAuthDataModule.WriteString('RecipientAddress', edtRecipientAddress.Text);
  EmailOAuthDataModule.WriteString('RecipientName', edtRecipientName.Text);
end;

procedure TForm2.btnSendViaRESTClick(Sender: TObject);
begin
  EmailOAuthDataModule.SendEmailUsingREST(edtFromAddress.Text, edtFromName.Text,
                          edtRecipientAddress.Text, edtRecipientName.Text,
                          edtSubject.Text, mmoBody.Text, '');
  EmailOAuthDataModule.WriteString('FromName', edtFromName.Text);
  EmailOAuthDataModule.WriteString('Subject', edtSubject.Text);
  EmailOAuthDataModule.WriteString('RecipientAddress', edtRecipientAddress.Text);
  EmailOAuthDataModule.WriteString('RecipientName', edtRecipientName.Text);
end;

procedure TForm2.LogMsg(const msg: string);
begin
  mmoLogging.Lines.Add(msg);
end;

procedure TForm2.rgEmailProvidersClick(Sender: TObject);
begin
  EmailOAuthDataModule.SelectedProvider := rgEmailProviders.ItemIndex;
  EmailOAuthDataModule.Provider := Providers[rgEmailProviders.ItemIndex];
  EmailOAuthDataModule.SetupAuthenticator;
  edtFromAddress.Text := EmailOAuthDataModule.SendAddress;
  UpdateButtonsEnabled;
end;

end.
