unit REST.Authenticator.EnhancedOAuth;

interface

uses
    System.SysUtils
  , REST.Authenticator.OAuth
  ;

type
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
  , Dialogs
  , IdSASL.Oauth.XOAUTH2
  , IdSASL.Oauth.OAuth2Bearer
  , Globals
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

end.
