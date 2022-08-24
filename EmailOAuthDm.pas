unit EmailOAuthDm;

interface

uses
    System.SysUtils
  , System.Classes
  , Winapi.ShellAPI
  , IniFiles
  , REST.Authenticator.OAuth
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
  ;

type
  TAuthType = class of TIdSASL;

  TOnLog = procedure(log: string) of object;

  TProviderInfo = record
    AuthenticationType : TAuthType;
    AuthorizationEndpoint : string;
    AccessTokenEndpoint : string;
    LogoutEndpoint : string;
    ClientID : String;
    ClientSecret : string;
    ClientAccount : string;
    Scopes : string;
    SmtpHost : string;
    SmtpPort : Integer;
    PopHost : string;
    PopPort : Integer;
    ImapHost : string;
    ImapPort : Integer;
    AuthName : string;
    TLS : TIdUseTLS;
    TwoLinePOPFormat: Boolean;
  end;

  TEnhancedOAuth2Authenticator = class (TOAuth2Authenticator)
  private
    procedure RequestAccessToken;
  public
    IDToken : string;
    procedure ChangeAuthCodeToAccesToken;
    procedure RefreshAccessTokenIfRequired;
  end;


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
    FIsAuthenticated : boolean;
    procedure DoLog(msg: String);
  public
    { Public declarations }
    OnLog: TOnLog;
    SelectedProvider : Integer;
    function IsAuthenticated: boolean;
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
  , IdSASLXOAUTH
  , IdOAuth2Bearer
  , Globals
  , REST.Client
  , REST.Consts
  , REST.Types
  ;

const
  clientredirect = 'http://localhost:2132';

  Providers : array[0..2] of TProviderInfo =
  (
    (  AuthenticationType : TIdSASLXOAuth;
       AuthorizationEndpoint : 'https://accounts.google.com/o/oauth2/auth';
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
              Scopes : 'wl.imap wl.offline_access';
       SmtpHost : 'smtp.office365.com';
       SmtpPort : 587;
       PopHost : 'outlook.office365.com';
       PopPort : 995;
       ImapHost : 'outlook.office365.com';
       ImapPort : 993;
       AuthName : 'Hotmail';
       TLS : utUseExplicitTLS;
       TwoLinePOPFormat: True
    )
  );


const
  SClientIDNeeded = 'An ClientID is needed before a token can be requested';
  SRefreshTokenNeeded = 'An Refresh Token is needed before an Access Token can be requested';

procedure TEnhancedOAuth2Authenticator.RefreshAccessTokenIfRequired;
begin
  if AccessTokenExpiry < now then
  begin
    RequestAccessToken;
  end;
end;

procedure TEnhancedOAuth2Authenticator.RequestAccessToken;
var
  LClient: TRestClient;
  LRequest: TRESTRequest;
  paramBody: TRESTRequestParameter;
  LToken: string;
  LIntValue: int64;
  url : TURI;
begin

  // we do need an clientid here, because we want
  // to send it to the servce and exchange the code into an
  // access-token.
  if ClientID = '' then
    raise EOAuth2Exception.Create(SClientIDNeeded);

  if RefreshToken = '' then
    raise EOAuth2Exception.Create(SRefreshTokenNeeded);

  LClient := TRestClient.Create(AccessTokenEndpoint);
  try
    LRequest := TRESTRequest.Create(LClient); // The LClient now "owns" the Request and will free it.
    LRequest.Method := TRESTRequestMethod.rmPOST;

    LRequest.AddAuthParameter('refresh_token', RefreshToken, TRESTRequestParameterKind.pkGETorPOST);
    LRequest.AddAuthParameter('client_id', ClientID, TRESTRequestParameterKind.pkGETorPOST);
    LRequest.AddAuthParameter('client_secret', ClientSecret, TRESTRequestParameterKind.pkGETorPOST);
    LRequest.AddAuthParameter('grant_type', 'refresh_token', TRESTRequestParameterKind.pkGETorPOST);
    url := TURI.Create('http://localhost');
    url.AddParameter('grant_type', 'refresh_token');
    url.AddParameter('refresh_token', RefreshToken);
    url.AddParameter('client_id', ClientID);
    url.AddParameter('client_secret', ClientSecret);
    paramBody := LRequest.Params.AddItem;
    paramBody.Value := url.Query;
    paramBody.Kind := pkREQUESTBODY;
    paramBody.Options := [poDoNotEncode];
    paramBody.ContentType := TRESTContentType.ctAPPLICATION_X_WWW_FORM_URLENCODED;

    LRequest.Execute;

    if LRequest.Response.GetSimpleValue('access_token', LToken) then
      AccessToken := LToken;
    if LRequest.Response.GetSimpleValue('refresh_token', LToken) then
      RefreshToken := LToken;
    if LRequest.Response.GetSimpleValue('id_token', LToken) then
      IDToken := LToken;

    // detect token-type. this is important for how using it later
    if LRequest.Response.GetSimpleValue('token_type', LToken) then
      TokenType := OAuth2TokenTypeFromString(LToken);

    // if provided by the service, the field "expires_in" contains
    // the number of seconds an access-token will be valid
    if LRequest.Response.GetSimpleValue('expires_in', LToken) then
    begin
      LIntValue := StrToIntdef(LToken, -1);
      if (LIntValue > -1) then
        AccessTokenExpiry := IncSecond(Now, LIntValue)
      else
        AccessTokenExpiry := 0.0;
    end;

    // an authentication-code may only be used once.
    // if we succeeded here and got an access-token, then
    // we do clear the auth-code as is is not valid anymore
    // and also not needed anymore.
    if (AccessToken <> '') then
    begin
      AuthCode := '';
    end;
  finally
    LClient.DisposeOf;
  end;
end;


// This function is basically a copy of the ancestor... but is need so we can also get the id_token value.
procedure TEnhancedOAuth2Authenticator.ChangeAuthCodeToAccesToken;
var
  LClient: TRestClient;
  LRequest: TRESTRequest;
  paramBody : TRESTRequestParameter;
  LToken: string;
  LIntValue: int64;
  url : TURI;
begin

  // we do need an authorization-code here, because we want
  // to send it to the servce and exchange the code into an
  // access-token.
  if AuthCode = '' then
    raise EOAuth2Exception.Create(SAuthorizationCodeNeeded);

  LClient := TRestClient.Create(AccessTokenEndpoint);
  try
    LRequest := TRESTRequest.Create(LClient); // The LClient now "owns" the Request and will free it.
    LRequest.Method := TRESTRequestMethod.rmPOST;
    url := TURI.Create('http://localhost');
    url.AddParameter('grant_type', 'authorization_code');
    url.AddParameter('code', AuthCode);
    url.AddParameter('client_id', ClientID);
    url.AddParameter('client_secret', ClientSecret);
    url.AddParameter('redirect_uri', RedirectionEndpoint);

    paramBody := LRequest.Params.AddItem;
    paramBody.Value := url.Query;
    paramBody.Kind := pkREQUESTBODY;
    paramBody.Options := [poDoNotEncode];
    paramBody.ContentType := TRESTContentType.ctAPPLICATION_X_WWW_FORM_URLENCODED;


    LRequest.Execute;

    if LRequest.Response.GetSimpleValue('access_token', LToken) then
      AccessToken := LToken;
    if LRequest.Response.GetSimpleValue('refresh_token', LToken) then
      RefreshToken := LToken;
    if LRequest.Response.GetSimpleValue('id_token', LToken) then
      IDToken := LToken;


    // detect token-type. this is important for how using it later
    if LRequest.Response.GetSimpleValue('token_type', LToken) then
      TokenType := OAuth2TokenTypeFromString(LToken);

    // if provided by the service, the field "expires_in" contains
    // the number of seconds an access-token will be valid
    if LRequest.Response.GetSimpleValue('expires_in', LToken) then
    begin
      LIntValue := StrToIntdef(LToken, -1);
      if (LIntValue > -1) then
        AccessTokenExpiry := IncSecond(Now, LIntValue)
      else
        AccessTokenExpiry := 0.0;
    end;

    // an authentication-code may only be used once.
    // if we succeeded here and got an access-token, then
    // we do clear the auth-code as is is not valid anymore
    // and also not needed anymore.
    if (AccessToken <> '') then
      AuthCode := '';
  finally
    LClient.DisposeOf;
  end;

end;

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
  LTokenName := Providers[SelectedProvider].AuthName + 'Token';
  FIniSettings.WriteString('Authentication', LTokenName, FOAuth2_Enhanced.RefreshToken);
  DoLog('Authenticated via OAUTH2');
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
  if SelectedProvider = 0 then
    uri.AddParameter('access_type', 'offline');  // For Google to get refresh_token

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
  LTokenName := Providers[SelectedProvider].AuthName + 'Token';
  FIniSettings.DeleteKey('Authentication', LTokenName);
  EmailOAuthDataModule.SetupAuthenticator;
end;

procedure TEmailOAuthDataModule.SendMessage(Path: String);
var
  IdMessage: TIdMessage;
  xoauthSASL : TIdSASLListEntry;
begin
  IdSMTP1.AuthType := satNone;

  // if we only have refresh_token or access token has expired
  // request new access_token to use with request
  FOAuth2_Enhanced.ClientID := Providers[SelectedProvider].ClientID;
  FOAuth2_Enhanced.ClientSecret := Providers[SelectedProvider].ClientSecret;

  FOAuth2_Enhanced.RefreshAccessTokenIfRequired;

  DoLog('refresh_token=' + FOAuth2_Enhanced.RefreshToken);
  DoLog('access_token=' + FOAuth2_Enhanced.AccessToken);

  if FOAuth2_Enhanced.AccessToken.Length = 0 then
  begin
    DoLog('Failed to authenticate properly');
    Exit;
  end;

  IdSMTP1.Host := Providers[SelectedProvider].SmtpHost;
  IdSMTP1.UseTLS := Providers[SelectedProvider].TLS;
  IdSMTP1.Port := Providers[SelectedProvider].SmtpPort;



  xoauthSASL := IdSMTP1.SASLMechanisms.Add;
  xoauthSASL.SASL := Providers[SelectedProvider].AuthenticationType.Create(nil);

  if xoauthSASL.SASL is TIdOAuth2Bearer then
  begin
    TIdOAuth2Bearer(xoauthSASL.SASL).Token := FOAuth2_Enhanced.AccessToken;
    TIdOAuth2Bearer(xoauthSASL.SASL).Host := IdSMTP1.Host;
    TIdOAuth2Bearer(xoauthSASL.SASL).Port := IdSMTP1.Port;
    TIdOAuth2Bearer(xoauthSASL.SASL).User := Providers[SelectedProvider].ClientAccount;
  end
  else if xoauthSASL.SASL is TIdSASLXOAuth then
  begin
    TIdSASLXOAuth(xoauthSASL.SASL).Token := FOAuth2_Enhanced.AccessToken;
    TIdSASLXOAuth(xoauthSASL.SASL).User := Providers[SelectedProvider].ClientAccount;
  end;
  IdSSLIOHandlerSocketSMTP.SSLOptions.SSLVersions := [sslvTLSv1_2];

  IdSMTP1.Connect;

  IdSMTP1.AuthType := satSASL;
  IdSMTP1.Authenticate;

  IdMessage := TIdMessage.Create(Self);
  IdMessage.From.Address := Providers[SelectedProvider].ClientAccount;
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
  FOAuth2_Enhanced.ClientID := Providers[SelectedProvider].ClientID;
  FOAuth2_Enhanced.ClientSecret := Providers[SelectedProvider].ClientSecret;
  FOAuth2_Enhanced.RefreshAccessTokenIfRequired;

  if FOAuth2_Enhanced.AccessToken.Length = 0 then
  begin
    DoLog('Failed to authenticate properly');
    Exit;
  end;

  IdIMAP.Host := Providers[SelectedProvider].ImapHost;
  IdIMAP.Port := Providers[SelectedProvider].ImapPort;
  IdIMAP.UseTLS := Providers[SelectedProvider].TLS;

  xoauthSASL := IdIMAP.SASLMechanisms.Add;
  xoauthSASL.SASL := Providers[SelectedProvider].AuthenticationType.Create(nil);

  if xoauthSASL.SASL is TIdOAuth2Bearer then
  begin
    TIdOAuth2Bearer(xoauthSASL.SASL).Token := FOAuth2_Enhanced.AccessToken;
    TIdOAuth2Bearer(xoauthSASL.SASL).Host := IdIMAP.Host;
    TIdOAuth2Bearer(xoauthSASL.SASL).Port := IdIMAP.Port;
    TIdOAuth2Bearer(xoauthSASL.SASL).User := Providers[SelectedProvider].ClientAccount;
  end
  else if xoauthSASL.SASL is TIdSASLXOAuth then
  begin
    TIdSASLXOAuth(xoauthSASL.SASL).Token := FOAuth2_Enhanced.AccessToken;
    TIdSASLXOAuth(xoauthSASL.SASL).User := Providers[SelectedProvider].ClientAccount;
  end;

  IdIMAP.AuthType := iatSASL;
  IdIMAP.Connect;

  mailboxes := TStringList.Create;
  try
    IdImap.ListMailBoxes(mailboxes);
    DoLog(mailboxes.Text);
  finally
    FreeAndNil(mailboxes);
  end;

  IdIMAP.SelectMailBox('[Gmail]/All Mail');
  msgCount:= IdIMAP.MailBox.TotalMsgs;
  ShowMessage(msgCount.ToString + ' Messages available for download');

  IdIMAP.Disconnect;
end;

procedure TEmailOAuthDataModule.CheckPOP;
var
  xoauthSASL : TIdSASLListEntry;
  msgCount : Integer;
begin

  DoLog('refresh_token=' + FOAuth2_Enhanced.RefreshToken);
  DoLog('access_token=' + FOAuth2_Enhanced.AccessToken);

  // if we only have refresh_token or access token has expired
  // request new access_token to use with request
  FOAuth2_Enhanced.ClientID := Providers[SelectedProvider].ClientID;
  FOAuth2_Enhanced.ClientSecret := Providers[SelectedProvider].ClientSecret;
  FOAuth2_Enhanced.RefreshAccessTokenIfRequired;

  if FOAuth2_Enhanced.AccessToken.Length = 0 then
  begin
    DoLog('Failed to authenticate properly');
    Exit;
  end;

  IdPOP3.Host := Providers[SelectedProvider].PopHost;
  IdPOP3.Port := Providers[SelectedProvider].PopPort;
  IdPOP3.UseTLS := utUseRequireTLS;//Providers[SelectedProvider].TLS;

  xoauthSASL := IdPOP3.SASLMechanisms.Add;
  xoauthSASL.SASL := Providers[SelectedProvider].AuthenticationType.Create(nil);

  if xoauthSASL.SASL is TIdOAuth2Bearer then
  begin
    TIdOAuth2Bearer(xoauthSASL.SASL).Token := FOAuth2_Enhanced.AccessToken;
    TIdOAuth2Bearer(xoauthSASL.SASL).Host := IdPOP3.Host;
    TIdOAuth2Bearer(xoauthSASL.SASL).Port := IdPOP3.Port;
    TIdOAuth2Bearer(xoauthSASL.SASL).User := Providers[SelectedProvider].ClientAccount;
  end
  else if xoauthSASL.SASL is TIdSASLXOAuth then
  begin
    TIdSASLXOAuth(xoauthSASL.SASL).Token := FOAuth2_Enhanced.AccessToken;
    TIdSASLXOAuth(xoauthSASL.SASL).User := Providers[SelectedProvider].ClientAccount;
    TIdSASLXOAuth(xoauthSASL.SASL).TwoLinePopFormat := Providers[SelectedProvider].TwoLinePOPFormat;
  end;

  IdPOP3.AuthType := patSASL;
  IdPOP3.UseTLS := utUseImplicitTLS;
  IdPOP3.Connect;
  IdPOP3.Login;

  msgCount := IdPOP3.CheckMessages;

  ShowMessage(msgCount.ToString + ' Messages available for download');

  IdPOP3.Disconnect;
end;


procedure TEmailOAuthDataModule.SetupAuthenticator;
var
  LTokenName : string;
begin
  FOAuth2_Enhanced.ClientID := Providers[SelectedProvider].ClientID;
  FOAuth2_Enhanced.ClientSecret := Providers[SelectedProvider].Clientsecret;
  FOAuth2_Enhanced.Scope := Providers[SelectedProvider].Scopes;
  FOAuth2_Enhanced.RedirectionEndpoint := clientredirect;
  FOAuth2_Enhanced.AuthorizationEndpoint := Providers[SelectedProvider].AuthorizationEndpoint;
  FOAuth2_Enhanced.AccessTokenEndpoint := Providers[SelectedProvider].AccessTokenEndpoint;

  LTokenName := Providers[SelectedProvider].AuthName + 'Token';
  FOAuth2_Enhanced.RefreshToken := FIniSettings.ReadString('Authentication', LTokenName, '');
  LTokenName := Providers[SelectedProvider].AuthName + 'Token';
end;

end.
