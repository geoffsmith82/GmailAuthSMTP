unit EmailOAuthDm;

interface

uses
    System.SysUtils
  , System.Classes
  , Winapi.ShellAPI
  , IniFiles
  , REST.Authenticator.EnhancedOAuth
  , IdIntercept
  , IdGlobal
  , IdContext
  , IdCustomHTTPServer
  , IdCustomTCPServer
  , IdHTTPServer
  , IdSMTPBase
  , IdSMTP
  , IdTCPConnection
  , IdTCPClient
  , IdExplicitTLSClientServerBase
  , IdSASLCollection
  , IdMessage
  , IdMessageClient
  , IdPOP3
  , IdBaseComponent
  , IdComponent
  , IdIOHandler
  , IdIOHandlerSocket
  , IdIOHandlerStack
  , IdSSL
  , IdSSLOpenSSL
  , IdIMAP4
  , IdSASL
  , IdSASL.OAuth.Base
  , Email.Demo.Types
  ;

type
  TEmailOAuthDataModule = class(TDataModule)
    IdSSLIOHandlerSocketPOP: TIdSSLIOHandlerSocketOpenSSL;
    IdPOP3: TIdPOP3;
    IdSMTP1: TIdSMTP;
    IdConnectionInterceptSMTP: TIdConnectionIntercept;
    IdSSLIOHandlerSocketSMTP: TIdSSLIOHandlerSocketOpenSSL;
    IdHTTPServer1: TIdHTTPServer;
    IdConnectionInterceptIMAP: TIdConnectionIntercept;
    IdConnectionPOP: TIdConnectionIntercept;
    IdIMAP: TIdIMAP4;
    IdSSLIOHandlerSocketIMAP: TIdSSLIOHandlerSocketOpenSSL;
    procedure DataModuleDestroy(Sender: TObject);
    procedure DataModuleCreate(Sender: TObject);
    procedure IdConnectionReceive(ASender: TIdConnectionIntercept; var ABuffer: TIdBytes);
    procedure IdConnectionSend(ASender: TIdConnectionIntercept; var ABuffer: TIdBytes);
    procedure IdHTTPServer1CommandGet(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
  private
    { Private declarations }
    FOAuth2_Enhanced : TEnhancedOAuth2Authenticator;
    FIniSettings : TIniFile;
    FIsAuthenticated : Boolean;
    procedure DoLog(msg: String);
  public
    { Public declarations }
    OnLog: TOnLog;
    SelectedProvider : Integer;
    Provider : TProviderInfo;
    function IsAuthenticated: Boolean;
    function HasRefreshToken: Boolean;
    procedure Authenticate;
    procedure ClearAuthentication;
    procedure SetupAuthenticator;
    procedure SendMessage(Path: String);
    procedure CheckIMAP;
    procedure CheckPOP;
  end;

var
  EmailOAuthDataModule: TEmailOAuthDataModule;

implementation

{%CLASSGROUP 'System.Classes.TPersistent'}

{$R *.dfm}

uses
  System.NetEncoding
  , System.Net.URLClient
  , System.DateUtils
  , Dialogs
  , Globals
  , REST.Client
  , REST.Consts
  , REST.Types
  ;

const
  clientredirect = 'http://localhost:2132';

procedure TEmailOAuthDataModule.DataModuleCreate(Sender: TObject);
var
  LFilename : string;
begin
  LFilename := ChangeFileExt(ParamStr(0),'.ini');
  FIniSettings := TIniFile.Create(LFilename);
  FOAuth2_Enhanced := TEnhancedOAuth2Authenticator.Create(nil);
  IdHTTPServer1.Active := True;
end;

procedure TEmailOAuthDataModule.DataModuleDestroy(Sender: TObject);
begin
  FreeAndNil(FOAuth2_Enhanced);
end;

procedure TEmailOAuthDataModule.DoLog(msg: String);
begin
  if Assigned(OnLog) then
    OnLog(msg);
end;

function TEmailOAuthDataModule.HasRefreshToken: Boolean;
begin
  Result := not FOAuth2_Enhanced.RefreshToken.IsEmpty;
end;

procedure TEmailOAuthDataModule.IdConnectionReceive(ASender: TIdConnectionIntercept; var ABuffer: TIdBytes);
begin
  DoLog('R:' + TEncoding.ASCII.GetString(ABuffer));
end;

procedure TEmailOAuthDataModule.IdConnectionSend(ASender: TIdConnectionIntercept; var ABuffer: TIdBytes);
begin
  DoLog('S:' + TEncoding.ASCII.GetString(ABuffer));
end;

procedure TEmailOAuthDataModule.IdHTTPServer1CommandGet(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  LCode: string;
  LURL : TURI;
  LTokenName : string;
begin
  if ARequestInfo.QueryParams = '' then
    Exit;
  LURL := TURI.Create('https://localhost/?' + ARequestInfo.QueryParams);
  try
    LCode := LURL.ParameterByName['code'];
  except
    Exit;
  end;
  FOAuth2_Enhanced.AuthCode := LCode;
  FOAuth2_Enhanced.ChangeAuthCodeToAccesToken;
  LTokenName := Provider.AuthName + 'Token';
  FIniSettings.WriteString('Authentication', LTokenName, FOAuth2_Enhanced.RefreshToken);
  DoLog('Authenticated via OAUTH2');
  DoLog(FOAuth2_Enhanced.RefreshToken);
  SetupAuthenticator;
end;

function TEmailOAuthDataModule.IsAuthenticated: boolean;
begin
  Result := FIsAuthenticated;
end;

procedure TEmailOAuthDataModule.Authenticate;
var
  uri : TURI;
begin
  uri := TURI.Create(FOAuth2_Enhanced.AuthorizationRequestURI);

  ShellExecute(0,
    'open',
    PChar(uri.ToString),
    nil,
    nil,
    0
  );
end;

procedure TEmailOAuthDataModule.ClearAuthentication;
var
  LTokenName : string;
begin
  // Delete persistent Refresh_token.  Note
  //  - This probably should have a logout function called on it
  //  - The token should be stored in an encrypted way ... but this is just a demo.
  LTokenName := Provider.AuthName + 'Token';
  FIniSettings.DeleteKey('Authentication', LTokenName);
  SetupAuthenticator;
end;

procedure TEmailOAuthDataModule.SendMessage(Path: String);
var
  IdMessage: TIdMessage;
  xoauthSASL : TIdSASLListEntry;
begin
  // if we only have refresh_token or access token has expired
  // request new access_token to use with request
  FOAuth2_Enhanced.ClientID := Provider.ClientID;
  FOAuth2_Enhanced.ClientSecret := Provider.ClientSecret;

  FOAuth2_Enhanced.RefreshAccessTokenIfRequired;

  DoLog('refresh_token=' + FOAuth2_Enhanced.RefreshToken);
  DoLog('access_token=' + FOAuth2_Enhanced.AccessToken);

  if FOAuth2_Enhanced.AccessToken.Length = 0 then
  begin
    DoLog('Failed to authenticate properly');
    Exit;
  end;

  IdSMTP1.Host := Provider.SmtpHost;
  IdSMTP1.UseTLS := Provider.TLS;
  IdSMTP1.Port := Provider.SmtpPort;

  xoauthSASL := IdSMTP1.SASLMechanisms.Add;
  xoauthSASL.SASL := Provider.AuthenticationType.Create(nil);

  TIdSASLOAuthBase(xoauthSASL.SASL).Token := FOAuth2_Enhanced.AccessToken;
  TIdSASLOAuthBase(xoauthSASL.SASL).User := Provider.ClientAccount;

  IdSSLIOHandlerSocketSMTP.SSLOptions.SSLVersions := [sslvTLSv1_2];

  IdSMTP1.Connect;

  IdSMTP1.AuthType := satSASL;
  IdSMTP1.Authenticate;

  IdMessage := TIdMessage.Create(Self);
  IdMessage.From.Address := Provider.ClientAccount;
  IdMessage.From.Name := clientname;
  IdMessage.ReplyTo.EMailAddresses := IdMessage.From.Address;
  IdMessage.Recipients.Add.Text := clientsendtoaddress;
  IdMessage.Subject := 'Hello World';
  IdMessage.Body.Text := 'Hello Body';

  IdSMTP1.Send(IdMessage);
  IdSMTP1.Disconnect;
  ShowMessage('Message Sent');
end;

procedure TEmailOAuthDataModule.CheckIMAP;
var
  xoauthSASL : TIdSASLListEntry;
  msgCount : Integer;
  mailboxes : TStringList;
begin
  DoLog('refresh_token=' + FOAuth2_Enhanced.RefreshToken);
  DoLog('access_token=' + FOAuth2_Enhanced.AccessToken);

  // if we only have refresh_token or access token has expired
  // request new access_token to use with request
  FOAuth2_Enhanced.ClientID := Provider.ClientID;
  FOAuth2_Enhanced.ClientSecret := Provider.ClientSecret;
  FOAuth2_Enhanced.RefreshAccessTokenIfRequired;

  if FOAuth2_Enhanced.AccessToken.Length = 0 then
  begin
    DoLog('Failed to authenticate properly');
    Exit;
  end;

  IdIMAP.Host := Provider.ImapHost;
  IdIMAP.Port := Provider.ImapPort;
  IdIMAP.UseTLS := Provider.TLS;

  xoauthSASL := IdIMAP.SASLMechanisms.Add;
  xoauthSASL.SASL := Provider.AuthenticationType.Create(nil);

  TIdSASLOAuthBase(xoauthSASL.SASL).Token := FOAuth2_Enhanced.AccessToken;
  TIdSASLOAuthBase(xoauthSASL.SASL).User := Provider.ClientAccount;

  IdIMAP.AuthType := iatSASL;
  IdIMAP.Connect;

  mailboxes := TStringList.Create;
  try
    IdImap.ListMailBoxes(mailboxes);
    DoLog(mailboxes.Text);
  finally
    FreeAndNil(mailboxes);
  end;

{
  IdIMAP.SelectMailBox('[Gmail]/All Mail');
  msgCount:= IdIMAP.MailBox.TotalMsgs;
  ShowMessage(msgCount.ToString + ' Messages available for download');
}
  IdIMAP.Disconnect;
end;

procedure TEmailOAuthDataModule.CheckPOP;
const
  ST_OK = '+OK';
  ST_SASLCONTINUE = '+';  {Do not translate}
var
  xoauthSASL : TIdSASLListEntry;
  msgCount : Integer;
begin
  IdPOP3.Disconnect;
  IdPOP3.AutoLogin := False;
  DoLog('refresh_token=' + FOAuth2_Enhanced.RefreshToken);
  DoLog('access_token=' + FOAuth2_Enhanced.AccessToken);

  // if we only have refresh_token or access token has expired
  // request new access_token to use with request
  FOAuth2_Enhanced.ClientID := Provider.ClientID;
  FOAuth2_Enhanced.ClientSecret := Provider.ClientSecret;
  FOAuth2_Enhanced.RefreshAccessTokenIfRequired;

  if FOAuth2_Enhanced.AccessToken.Length = 0 then
  begin
    DoLog('Failed to authenticate properly');
    Exit;
  end;

  IdPOP3.Host := Provider.PopHost;
  IdPOP3.Port := Provider.PopPort;
  IdPOP3.UseTLS := utUseImplicitTLS;//Providers[SelectedProvider].TLS;

  xoauthSASL := IdPOP3.SASLMechanisms.Add;
  xoauthSASL.SASL := Provider.AuthenticationType.Create(nil);

  TIdSASLOAuthBase(xoauthSASL.SASL).Token := FOAuth2_Enhanced.AccessToken;
  TIdSASLOAuthBase(xoauthSASL.SASL).User := Provider.ClientAccount;

  IdPOP3.AuthType := patSASL;
  IdPOP3.UseTLS := utUseImplicitTLS;
  IdPOP3.Connect;
  IdPOP3.CAPA;
//  IdPOP3.Login;
  IdPOP3.SASLMechanisms.LoginSASL('AUTH', IdPOP3.Host, 'pop', [ST_OK], [ST_SASLCONTINUE], IdPOP3, IdPOP3.Capabilities, 'SASL', False); {do not localize}

  msgCount := IdPOP3.CheckMessages;

  ShowMessage(msgCount.ToString + ' Messages available for download');

  IdPOP3.Disconnect;
end;

procedure TEmailOAuthDataModule.SetupAuthenticator;
var
  LTokenName : string;
begin
  FOAuth2_Enhanced.ClientID := Provider.ClientID;
  FOAuth2_Enhanced.ClientSecret := Provider.Clientsecret;
  FOAuth2_Enhanced.Scope := Provider.Scopes;
  FOAuth2_Enhanced.RedirectionEndpoint := clientredirect;
  FOAuth2_Enhanced.AuthorizationEndpoint := Provider.AuthorizationEndpoint;
  FOAuth2_Enhanced.AccessTokenEndpoint := Provider.AccessTokenEndpoint;

  LTokenName := Provider.AuthName + 'Token';
  FOAuth2_Enhanced.RefreshToken := FIniSettings.ReadString('Authentication', LTokenName, '');
  FOAuth2_Enhanced.AccessToken := '';
  FOAuth2_Enhanced.AccessTokenExpiry := 0;
  IdSMTP1.Disconnect;
  IdPOP3.Disconnect;
  IdIMAP.Disconnect;
end;

end.
