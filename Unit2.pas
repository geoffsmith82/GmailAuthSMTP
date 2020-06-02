unit Unit2;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.StdCtrls,
  Vcl.Forms,
  Vcl.Dialogs,
  IdBaseComponent,
  IdComponent,
  IdTCPConnection,
  IdTCPClient,
  IdExplicitTLSClientServerBase,
  IdMessageClient,
  IdMessage,
  IdSMTPBase,
  IdSMTP,
  IdSASL,
  IdIOHandler,
  IdIOHandlerSocket,
  IdIOHandlerStack,
  IdSSL,
  IdSSLOpenSSL,
  IdIntercept,
  IdGlobal,
  Data.Bind.Components,
  Data.Bind.ObjectScope,
  REST.Client,
  IdCustomTCPServer,
  IdCustomHTTPServer,
  IdHTTPServer,
  REST.Authenticator.OAuth,
  IdContext,
  IdSASLCollection,
  IdSASLXOAUTH,
  IdOAuth2Bearer,
  Vcl.ExtCtrls,
  IdPOP3,
  IniFiles,
  Globals
  ;

type


  TEnhancedOAuth2Authenticator = class (TOAuth2Authenticator)
  private
    procedure RequestAccessToken;
  public
    IDToken : string;
    procedure ChangeAuthCodeToAccesToken;
    procedure RefreshAccessTokenIfRequired;
  end;

  TAuthType = class of TIdSASL;

  TProviderInfo = record
    AuthenticationType : TAuthType;
    AuthorizationEndpoint : string;
    AccessTokenEndpoint : string;
    ClientID : String;
    ClientSecret : string;
    ClientAccount : string;
    Scopes : string;
    SmtpHost : string;
    SmtpPort : Integer;
    PopHost : string;
    PopPort : Integer;
    AuthName : string;
    TLS : TIdUseTLS;
  end;

  TForm2 = class(TForm)
    IdSMTP1: TIdSMTP;
    IdSSLIOHandlerSocketSMTP: TIdSSLIOHandlerSocketOpenSSL;
    Memo1: TMemo;
    IdConnectionInterceptSMTP: TIdConnectionIntercept;
    btnAuthenticate: TButton;
    btnSendMsg: TButton;
    IdHTTPServer1: TIdHTTPServer;
    rgEmailProviders: TRadioGroup;
    IdPOP3: TIdPOP3;
    btnCheckMsg: TButton;
    IdConnectionPOP: TIdConnectionIntercept;
    IdSSLIOHandlerSocketPOP: TIdSSLIOHandlerSocketOpenSSL;
    btnClearAuthToken: TButton;
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnAuthenticateClick(Sender: TObject);
    procedure btnSendMsgClick(Sender: TObject);
    procedure btnCheckMsgClick(Sender: TObject);
    procedure btnClearAuthTokenClick(Sender: TObject);
    procedure IdConnectionInterceptSMTPReceive(ASender: TIdConnectionIntercept; var ABuffer: TIdBytes);
    procedure IdConnectionInterceptSMTPSend(ASender: TIdConnectionIntercept; var ABuffer: TIdBytes);
    procedure IdHTTPServer1CommandGet(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
    procedure rgEmailProvidersClick(Sender: TObject);
  private
    { Private declarations }
    OAuth2_Enhanced : TEnhancedOAuth2Authenticator;
    IniSettings : TIniFile;
    procedure SetupAuthenticator;
  public
    { Public declarations }
  end;

const
  Providers : array[0..1] of TProviderInfo =
  (
    (  AuthenticationType : TIdOAuth2Bearer;
       AuthorizationEndpoint : 'https://accounts.google.com/o/oauth2/auth';
       AccessTokenEndpoint : 'https://accounts.google.com/o/oauth2/token';
       ClientID : google_clientid;
       ClientSecret : google_clientsecret;
       ClientAccount : google_clientAccount;  // your @gmail.com email address
       Scopes : 'https://mail.google.com/ openid';
       SmtpHost : 'smtp.gmail.com';
       SmtpPort : 465;
       PopHost : 'pop.gmail.com';
       PopPort : 995;
       AuthName : 'Google';
       TLS : utUseImplicitTLS
    ),
    (  AuthenticationType : TIdSASLXOAuth;
       AuthorizationEndpoint : 'https://login.live.com/oauth20_authorize.srf';
       AccessTokenEndpoint : 'https://login.live.com/oauth20_token.srf';
       ClientID : microsoft_clientid;
       ClientSecret : '';
       ClientAccount : microsoft_clientAccount; // your @live.com or @hotmail.com email address
       Scopes : 'wl.imap offline_access';
       SmtpHost : 'smtp.office365.com';
       SmtpPort : 587;
       PopHost : 'outlook.office365.com';
       PopPort : 995;
       AuthName : 'Microsoft';
       TLS : utUseImplicitTLS
    )
  );

const clientredirect = 'http://localhost:2132';

var
  Form2: TForm2;

implementation

{$R *.dfm}

uses
  System.NetEncoding,
  System.Net.URLClient,
  REST.Utils,
  Winapi.ShellAPI,
  REST.Consts,
  REST.Types,
  System.DateUtils
  ;

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
  LToken: string;
  LIntValue: int64;
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
  LToken: string;
  LIntValue: int64;
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
    // LRequest.Client := LClient; // unnecessary since the client "owns" the request it will assign the client

    LRequest.AddAuthParameter('code', AuthCode, TRESTRequestParameterKind.pkGETorPOST);
    LRequest.AddAuthParameter('client_id', ClientID, TRESTRequestParameterKind.pkGETorPOST);
    LRequest.AddAuthParameter('client_secret', ClientSecret, TRESTRequestParameterKind.pkGETorPOST);
    LRequest.AddAuthParameter('redirect_uri', RedirectionEndpoint, TRESTRequestParameterKind.pkGETorPOST);
    LRequest.AddAuthParameter('grant_type', 'authorization_code', TRESTRequestParameterKind.pkGETorPOST);

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

procedure TForm2.FormDestroy(Sender: TObject);
begin
  FreeAndNil(IniSettings);
  FreeAndNil(OAuth2_Enhanced);
end;

procedure TForm2.FormCreate(Sender: TObject);
var
  LFilename : string;
begin
  LFilename := ChangeFileExt(ParamStr(0),'.ini');
  IniSettings := TIniFile.Create(LFilename);

  OAuth2_Enhanced := TEnhancedOAuth2Authenticator.Create(nil);
  SetupAuthenticator;
end;

procedure TForm2.btnAuthenticateClick(Sender: TObject);
var
  uri : TURI;
begin
  uri := TURI.Create(OAuth2_Enhanced.AuthorizationRequestURI);
  if rgEmailProviders.ItemIndex = 0 then
    uri.AddParameter('access_type', 'offline');  // For Google to get refresh_token

  ShellExecute(Handle,
    'open',
    PChar(uri.ToString),
    nil,
    nil,
    0
  );
end;

procedure TForm2.btnSendMsgClick(Sender: TObject);
var
  IdMessage: TIdMessage;
  xoauthSASL : TIdSASLListEntry;
begin
  IdSMTP1.AuthType := satNone;

  // if we only have refresh_token or access token has expired
  // request new access_token to use with request
  OAuth2_Enhanced.RefreshAccessTokenIfRequired;

  Memo1.Lines.Add('refresh_token=' + OAuth2_Enhanced.RefreshToken);
  Memo1.Lines.Add('access_token=' + OAuth2_Enhanced.AccessToken);

  if OAuth2_Enhanced.AccessToken.Length = 0 then
  begin
    Memo1.Lines.Add('Failed to authenticate properly');
    Exit;
  end;

  IdSMTP1.Host := Providers[rgEmailProviders.ItemIndex].SmtpHost;
  IdSMTP1.Port := Providers[rgEmailProviders.ItemIndex].SmtpPort;
  IdSMTP1.UseTLS := Providers[rgEmailProviders.ItemIndex].TLS;

  xoauthSASL := IdSMTP1.SASLMechanisms.Add;
  xoauthSASL.SASL := Providers[rgEmailProviders.ItemIndex].AuthenticationType.Create(nil);

  if xoauthSASL.SASL is TIdOAuth2Bearer then
  begin
    TIdOAuth2Bearer(xoauthSASL.SASL).Token := OAuth2_Enhanced.AccessToken;
    TIdOAuth2Bearer(xoauthSASL.SASL).Host := IdSMTP1.Host;
    TIdOAuth2Bearer(xoauthSASL.SASL).Port := IdSMTP1.Port;
    TIdOAuth2Bearer(xoauthSASL.SASL).User := Providers[rgEmailProviders.ItemIndex].ClientAccount;
  end
  else if xoauthSASL.SASL is TIdSASLXOAuth then
  begin
    TIdSASLXOAuth(xoauthSASL.SASL).Token := OAuth2_Enhanced.AccessToken;
    TIdSASLXOAuth(xoauthSASL.SASL).User := Providers[rgEmailProviders.ItemIndex].ClientAccount;
  end;


  IdSMTP1.Connect;
  IdSMTP1.AuthType := satSASL;
  IdSMTP1.Authenticate;

  IdMessage := TIdMessage.Create(Self);
  IdMessage.From.Address := Providers[rgEmailProviders.ItemIndex].ClientAccount;
  IdMessage.From.Name := clientname;
  IdMessage.ReplyTo.EMailAddresses := IdMessage.From.Address;
  IdMessage.Recipients.Add.Text := clientsendtoaddress;
  IdMessage.Subject := 'Hello World';
  IdMessage.Body.Text := 'Hello Body';

  IdSMTP1.Send(IdMessage);

  IdSMTP1.Disconnect;
end;

procedure TForm2.btnCheckMsgClick(Sender: TObject);
var
  xoauthSASL : TIdSASLListEntry;
  msgCount : Integer;
begin

  Memo1.Lines.Add('refresh_token=' + OAuth2_Enhanced.RefreshToken);
  Memo1.Lines.Add('access_token=' + OAuth2_Enhanced.AccessToken);

  if OAuth2_Enhanced.AccessToken.Length = 0 then
  begin
    Memo1.Lines.Add('Failed to authenticate properly');
    Exit;
  end;

  IdPOP3.Host := Providers[rgEmailProviders.ItemIndex].PopHost;
  IdPOP3.Port := Providers[rgEmailProviders.ItemIndex].PopPort;
  IdPOP3.UseTLS := Providers[rgEmailProviders.ItemIndex].TLS;

  xoauthSASL := IdPOP3.SASLMechanisms.Add;
  xoauthSASL.SASL := Providers[rgEmailProviders.ItemIndex].AuthenticationType.Create(nil);

  if xoauthSASL.SASL is TIdOAuth2Bearer then
  begin
    TIdOAuth2Bearer(xoauthSASL.SASL).Token := OAuth2_Enhanced.AccessToken;
    TIdOAuth2Bearer(xoauthSASL.SASL).Host := IdPOP3.Host;
    TIdOAuth2Bearer(xoauthSASL.SASL).Port := IdPOP3.Port;
    TIdOAuth2Bearer(xoauthSASL.SASL).User := Providers[rgEmailProviders.ItemIndex].ClientAccount;
  end
  else if xoauthSASL.SASL is TIdSASLXOAuth then
  begin
    TIdSASLXOAuth(xoauthSASL.SASL).Token := OAuth2_Enhanced.AccessToken;
    TIdSASLXOAuth(xoauthSASL.SASL).User := Providers[rgEmailProviders.ItemIndex].ClientAccount;
  end;

  IdPOP3.AuthType := patSASL;
  IdPOP3.Connect;
  IdPOP3.CAPA;
  IdPOP3.Login;

  msgCount := IdPOP3.CheckMessages;

  ShowMessage(msgCount.ToString + ' Messages available for download');

  IdPOP3.Disconnect;
end;

procedure TForm2.btnClearAuthTokenClick(Sender: TObject);
var
  LTokenName : string;
begin
  // Delete persistent Refresh_token.  Note
  //  - This probably should have a logout function called on it
  //  - The token should be stored in an encrypted way ... but this is just a demo.
  LTokenName := Providers[rgEmailProviders.ItemIndex].AuthName + 'Token';
  IniSettings.DeleteKey('Authentication', LTokenName);
  SetupAuthenticator;
end;

procedure TForm2.IdConnectionInterceptSMTPReceive(ASender: TIdConnectionIntercept; var ABuffer: TIdBytes);
begin
  Memo1.Lines.Add('R:' + TEncoding.ASCII.GetString(ABuffer));
end;

procedure TForm2.IdConnectionInterceptSMTPSend(ASender: TIdConnectionIntercept; var ABuffer: TIdBytes);
begin
  Memo1.Lines.Add('S:' + TEncoding.ASCII.GetString(ABuffer));
end;

procedure TForm2.IdHTTPServer1CommandGet(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  LCode: string;
  LURL : TURI;
  LTokenName : string;
begin
  LURL := TURI.Create('https://localhost/?' + ARequestInfo.QueryParams);
  try
    LCode := LURL.ParameterByName['code'];
  except
    Exit;
  end;
  OAuth2_Enhanced.AuthCode := LCode;
  OAuth2_Enhanced.ChangeAuthCodeToAccesToken;
  LTokenName := Providers[rgEmailProviders.ItemIndex].AuthName + 'Token';
  IniSettings.WriteString('Authentication', LTokenName, OAuth2_Enhanced.RefreshToken);
  Memo1.Lines.Add('Authenticated via OAUTH2');
  SetupAuthenticator;
end;

procedure TForm2.rgEmailProvidersClick(Sender: TObject);
begin
  SetupAuthenticator;
end;

procedure TForm2.SetupAuthenticator;
var
  LTokenName : string;
begin
  OAuth2_Enhanced.ClientID := Providers[rgEmailProviders.ItemIndex].ClientID;
  OAuth2_Enhanced.ClientSecret := Providers[rgEmailProviders.ItemIndex].Clientsecret;
  OAuth2_Enhanced.Scope := Providers[rgEmailProviders.ItemIndex].Scopes;
  OAuth2_Enhanced.RedirectionEndpoint := clientredirect;
  OAuth2_Enhanced.AuthorizationEndpoint := Providers[rgEmailProviders.ItemIndex].AuthorizationEndpoint;
  OAuth2_Enhanced.AccessTokenEndpoint := Providers[rgEmailProviders.ItemIndex].AccessTokenEndpoint;

  LTokenName := Providers[rgEmailProviders.ItemIndex].AuthName + 'Token';
  OAuth2_Enhanced.RefreshToken := IniSettings.ReadString('Authentication', LTokenName, '');
  LTokenName := Providers[rgEmailProviders.ItemIndex].AuthName + 'Token';
  btnAuthenticate.Enabled := IniSettings.ReadString('Authentication', LTokenName, '').Length = 0;
  btnClearAuthToken.Enabled := not btnAuthenticate.Enabled;
end;

end.
