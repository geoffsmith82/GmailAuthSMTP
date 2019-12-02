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
  IdSMTPBase,
  IdSMTP,
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
  IdOAuth2Bearer
  ;

type
  TGoogleOAuth2Authenticator = class (TOAuth2Authenticator)
  public
    IDToken : string;
    procedure ChangeAuthCodeToAccesToken;
  end;

  TForm2 = class(TForm)
    IdSMTP1: TIdSMTP;
    IdSSLIOHandlerSocketOpenSSL1: TIdSSLIOHandlerSocketOpenSSL;
    Memo1: TMemo;
    IdConnectionIntercept1: TIdConnectionIntercept;
    Button1: TButton;
    Button2: TButton;
    IdHTTPServer1: TIdHTTPServer;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure IdConnectionIntercept1Receive(ASender: TIdConnectionIntercept; var ABuffer: TIdBytes);
    procedure IdConnectionIntercept1Send(ASender: TIdConnectionIntercept; var
        ABuffer: TIdBytes);
    procedure IdHTTPServer1CommandGet(AContext: TIdContext; ARequestInfo:
        TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
  private
    { Private declarations }
    OAuth2_GMail : TGoogleOAuth2Authenticator;
  public
    { Public declarations }
  end;




var
  Form2: TForm2;

implementation

{$R *.dfm}

uses
  System.NetEncoding,
  System.Net.URLClient,
  REST.Utils,
  Winapi.ShellAPI,
  IdMessage,
  REST.Consts,
  REST.Types,
  System.DateUtils,
  GmailGlobals
  ;

procedure TGoogleOAuth2Authenticator.ChangeAuthCodeToAccesToken;
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

procedure TForm2.FormCreate(Sender: TObject);
begin
  OAuth2_GMail := TGoogleOAuth2Authenticator.Create(nil);
  OAuth2_GMail.ClientID := clientid;
  OAuth2_GMail.ClientSecret := clientsecret;
  OAuth2_GMail.Scope := clientscope;
  OAuth2_GMail.RedirectionEndpoint := clientredirect;
  OAuth2_GMail.AuthorizationEndpoint := 'https://accounts.google.com/o/oauth2/auth';
  OAuth2_GMail.AccessTokenEndpoint := 'https://accounts.google.com/o/oauth2/token';
end;

procedure TForm2.Button1Click(Sender: TObject);
var
  uri : TURI;
begin
  uri := TURI.Create(OAuth2_GMail.AuthorizationRequestURI);
  uri.AddParameter('access_type', 'offline');
  ShellExecute(Handle,
    'open',
    PChar(OAuth2_GMail.AuthorizationRequestURI),
    nil,
    nil,
    0
  );
end;

procedure TForm2.Button2Click(Sender: TObject);
var
  IdMessage: TIdMessage;
  xoauthSASL : TIdSASLListEntry;
begin
  IdSMTP1.AuthType := satNone;

  Memo1.Lines.Add('refresh_token=' + OAuth2_GMail.RefreshToken);

  IdSMTP1.Host := 'smtp.gmail.com';
  IdSMTP1.Port := 465;
  IdSMTP1.UseTLS := utUseImplicitTLS;

  xoauthSASL := IdSMTP1.SASLMechanisms.Add;
  xoauthSASL.SASL := TIdOAuth2Bearer.Create(nil);
//  xoauthSASL.SASL := TIdSASLXOAuth.Create(nil);


  TIdOAuth2Bearer(xoauthSASL.SASL).Token := OAuth2_GMail.AccessToken;
  TIdOAuth2Bearer(xoauthSASL.SASL).Host := IdSMTP1.Host;
  TIdOAuth2Bearer(xoauthSASL.SASL).Port := IdSMTP1.Port;
  TIdOAuth2Bearer(xoauthSASL.SASL).User := clientaccount;


{
  TIdSASLXOAuth(xoauthSASL.SASL).Token := OAuth2_GMail.AccessToken;
  TIdSASLXOAuth(xoauthSASL.SASL).User := clientaccount;
}

  IdSMTP1.Connect;
  IdSMTP1.AuthType := satSASL;
  IdSMTP1.Authenticate;

  IdMessage := TIdMessage.Create(Self);
  IdMessage.From.Address := clientaccount;
  IdMessage.From.Name := clientname;
  IdMessage.ReplyTo.EMailAddresses := IdMessage.From.Address;
  IdMessage.Recipients.Add.Text := clientsendtoaddress;
  IdMessage.Subject := 'Hello World';
  IdMessage.Body.Text := 'Hello Body';

  IdSMTP1.Send(IdMessage);
end;

procedure TForm2.IdConnectionIntercept1Receive(ASender: TIdConnectionIntercept; var ABuffer: TIdBytes);
begin
  Memo1.Lines.Add('R:' + TEncoding.ASCII.GetString(ABuffer));
end;

procedure TForm2.IdConnectionIntercept1Send(ASender: TIdConnectionIntercept;
    var ABuffer: TIdBytes);
begin
  Memo1.Lines.Add('S:' + TEncoding.ASCII.GetString(ABuffer));
end;

procedure TForm2.IdHTTPServer1CommandGet(AContext: TIdContext; ARequestInfo:
    TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  LATPos: integer;
  LCode: string;
  LURL : string;
begin
  LURL := ARequestInfo.QueryParams;
  LATPos := Pos('code=', LURL);
  if (LATPos > 0) then
  begin
    LCode := Copy(LURL, LATPos + 5, Length(LURL));
    if (Pos('&', LCode) > 0) then
    begin
      LCode := Copy(LCode, 1, Pos('&', LCode) - 1);
      OAuth2_GMail.AuthCode := LCode;
      OAuth2_GMail.ChangeAuthCodeToAccesToken;
    end;
  end;
end;

end.
