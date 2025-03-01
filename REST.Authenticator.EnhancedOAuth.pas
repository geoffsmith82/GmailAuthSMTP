unit REST.Authenticator.EnhancedOAuth;

interface

uses
    System.SysUtils
  , REST.Authenticator.OAuth
  , System.JSON
  ;

type
  TJWTHeader = class
  private
    FHeaderJSON: TJSONObject;
  public
    constructor Create(const HeaderStr: string);
    destructor Destroy; override;
    function Algorithm: string;
    function TokenType: string;
    function KeyId: string;
  end;

  TJWTPayload = class
  private
    FPayloadJSON: TJSONObject;
  public
    constructor Create(const PayloadStr: string);
    destructor Destroy; override;
    function Subject: string;
    function Issuer: string;
    function Expiration: Int64;
    function ContainsKey(const Key: string): Boolean;
    function GetValue(const Key: string): string;
  end;

  TJWT = class
  private
    FHeader: TJWTHeader;
    FPayload: TJWTPayload;
    FSignature: string;
    function Base64UrlDecode(const Input: string): string;
  public
    constructor Create(const Token: string);
    destructor Destroy; override;
    function GetSignature: string;
    property Header: TJWTHeader read FHeader;
    property Payload: TJWTPayload read FPayload;
  end;

  TEnhancedOAuth2Authenticator = class (TOAuth2Authenticator)
  private
    procedure RequestAccessToken;
  public
    IDToken : string;
    procedure ChangeAuthCodeToAccesToken;
    procedure RefreshAccessTokenIfRequired;
    function AuthorizationRequestURI: string;
  end;

implementation

uses
    System.NetEncoding
  , System.Net.URLClient
  , System.DateUtils
  , IdSASL.Oauth.XOAUTH2
  , IdSASL.Oauth.OAuth2Bearer
  , REST.Client
  , REST.Consts
  , REST.Types
  ;


const
  SClientIDNeeded = 'An ClientID is needed before a token can be requested';
  SRefreshTokenNeeded = 'An Refresh Token is needed before an Access Token can be requested';

function TEnhancedOAuth2Authenticator.AuthorizationRequestURI: string;
var
  uri : TURI;
begin
  uri := TURI.Create(AuthorizationEndpoint);
  uri.AddParameter('response_type', OAuth2ResponseTypeToString(ResponseType));
  if ClientID <> '' then
    uri.AddParameter('client_id', ClientID);
  if RedirectionEndpoint <> '' then
    uri.AddParameter('redirect_uri', RedirectionEndpoint);
  if Scope <> '' then
    uri.AddParameter('scope', Scope);
  if LocalState <> '' then
    uri.AddParameter('state', LocalState);

  Result := uri.ToString;
end;

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

    url := TURI.Create('http://localhost');
    url.AddParameter('grant_type', 'refresh_token');
    url.AddParameter('refresh_token', RefreshToken);
    url.AddParameter('client_id', ClientID);
    if not ClientSecret.IsEmpty then
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
    FreeAndNil(LClient);
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
    FreeAndNil(LClient);
  end;
end;

{ TJWTHeader }

constructor TJWTHeader.Create(const HeaderStr: string);
begin
  FHeaderJSON := TJSONObject.ParseJSONValue(HeaderStr) as TJSONObject;
end;

destructor TJWTHeader.Destroy;
begin
  FHeaderJSON.Free;
  inherited;
end;

function TJWTHeader.Algorithm: string;
begin
  if Assigned(FHeaderJSON) and FHeaderJSON.TryGetValue<string>('alg', Result) then
    Exit;
  Result := '';
end;

function TJWTHeader.TokenType: string;
begin
  if Assigned(FHeaderJSON) and FHeaderJSON.TryGetValue<string>('typ', Result) then
    Exit;
  Result := '';
end;

function TJWTHeader.KeyId: string;
begin
  if Assigned(FHeaderJSON) and FHeaderJSON.TryGetValue<string>('kid', Result) then
    Exit;
  Result := '';
end;

{ TJWTPayload }

constructor TJWTPayload.Create(const PayloadStr: string);
begin
  FPayloadJSON := TJSONObject.ParseJSONValue(PayloadStr) as TJSONObject;
end;

destructor TJWTPayload.Destroy;
begin
  FPayloadJSON.Free;
  inherited;
end;

function TJWTPayload.Subject: string;
begin
  if Assigned(FPayloadJSON) and FPayloadJSON.TryGetValue<string>('sub', Result) then
    Exit;
  Result := '';
end;

function TJWTPayload.Issuer: string;
begin
  if Assigned(FPayloadJSON) and FPayloadJSON.TryGetValue<string>('iss', Result) then
    Exit;
  Result := '';
end;

function TJWTPayload.Expiration: Int64;
begin
  if Assigned(FPayloadJSON) and FPayloadJSON.TryGetValue<Int64>('exp', Result) then
    Exit;
  Result := 0;
end;

function TJWTPayload.ContainsKey(const Key: string): Boolean;
begin
  Result := Assigned(FPayloadJSON) and (FPayloadJSON.Values[Key] <> nil);
end;

function TJWTPayload.GetValue(const Key: string): string;
begin
  if Assigned(FPayloadJSON) and FPayloadJSON.TryGetValue<string>(Key, Result) then
    Exit;
  Result := '';
end;

{ TJWT }

constructor TJWT.Create(const Token: string);
var
  Parts: TArray<string>;
begin
  Parts := Token.Split(['.']);
  if Length(Parts) = 3 then
  begin
    FHeader := TJWTHeader.Create(Base64UrlDecode(Parts[0]));
    FPayload := TJWTPayload.Create(Base64UrlDecode(Parts[1]));
    FSignature := Parts[2];
  end
  else
    raise Exception.Create('Invalid JWT token format');
end;

destructor TJWT.Destroy;
begin
  FHeader.Free;
  FPayload.Free;
  inherited;
end;

function TJWT.Base64UrlDecode(const Input: string): string;
var
  Base64: string;
  Bytes: TBytes;
begin
  Base64 := Input;
  Base64 := Base64.Replace('-', '+').Replace('_', '/');
  while (Length(Base64) mod 4) <> 0 do
    Base64 := Base64 + '=';

  Bytes := TBase64Encoding.Base64.DecodeStringToBytes(Base64);
  Result := TEncoding.UTF8.GetString(Bytes);
end;

function TJWT.GetSignature: string;
begin
  Result := FSignature;
end;

end.
