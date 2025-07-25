unit EmailOAuthDm;

interface

uses
    System.SysUtils
  , System.Classes
  , System.JSON
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
  , IdText
  , IdMessageParts
  , IdAttachmentFile
  , IdMessageClient
  , IdEMailAddress
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
  , windows
  , REST.Types
  , REST.Client
  , Data.Bind.Components
  , Data.Bind.ObjectScope
  , IdCTypes
  , TaurusTLSHeaders_types
  , TaurusTLS_X509
  , TaurusTLS
  ;

type
  TEmailOAuthDataModule = class(TDataModule)
    IdSSLIOHandlerSocketPOP: TTaurusTLSIOHandlerSocket;
    IdPOP3: TIdPOP3;
    IdSMTP1: TIdSMTP;
    IdConnectionInterceptSMTP: TIdConnectionIntercept;
    IdSSLIOHandlerSocketSMTP: TTaurusTLSIOHandlerSocket;
    IdHTTPServer1: TIdHTTPServer;
    IdConnectionInterceptIMAP: TIdConnectionIntercept;
    IdConnectionPOP: TIdConnectionIntercept;
    IdIMAP: TIdIMAP4;
    IdSSLIOHandlerSocketIMAP: TTaurusTLSIOHandlerSocket;
    RESTResponseGraph: TRESTResponse;
    RESTRequestGraph: TRESTRequest;
    RESTClientGraph: TRESTClient;
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
    procedure DoLog(const msg: String);
    procedure ForceForegroundNoActivate(hWnd: THandle);
  public
    { Public declarations }
    SendAddress : string;
    OnLog: TOnLog;
    HWNDHandle : HWND;
    AppHandle : HWND;
    SelectedProvider : Integer;
    Provider : TMailProviderInfo;
    function IsAuthenticated: Boolean;
    function HasRefreshToken: Boolean;
    procedure Authenticate;
    procedure ClearAuthentication;
    procedure SetupAuthenticator;
    function ReadString(const Ident, Default: string): string;
    procedure WriteString(const Ident, Value: String);
    procedure SendMessage(const fromAddress: string;
                          const fromName:string;
                          const recepientAddress: string;
                          const recepientName: string;
                          const subject: string;
                          const body: string;
                          const Path: String);
    function SendEmailUsingREST(const fromAddress: string;
                          const fromName: string;
                          const recepientAddress: string;
                          const recepientName: string;
                          const subject: string;
                          const body: string;
                          const Path: string): Boolean;
    procedure SendEmailWithAttachment(const fromAddress: string;
                                      const fromName:string;
                                      const recepientAddress: string;
                                      const recepientName: string;
                                      const subjectText: string;
                                      const PlainBody, HtmlBody: string;
                                      const InlineImagePaths: array of string;
                                      const AttachmentPath: array of string);
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
  , REST.Consts
  ;

const
  clientredirect = 'http://localhost:2132';

procedure TEmailOAuthDataModule.ForceForegroundNoActivate(hWnd : THandle);

begin
 if IsIconic(AppHandle) then
  ShowWindow(AppHandle, SW_SHOWNOACTIVATE);
 SetWindowPos(hWnd, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOSIZE or SWP_NOACTIVATE or SWP_NOMOVE);
 SetWindowPos(hWnd, HWND_NOTOPMOST, 0, 0, 0, 0, SWP_NOSIZE or SWP_NOACTIVATE or SWP_NOMOVE);
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

procedure TEmailOAuthDataModule.DoLog(const msg: String);
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
  var jwt := TJWT.Create(FOAuth2_Enhanced.IDToken);
  if jwt.Payload.ContainsKey('email') then
  begin
    FIniSettings.WriteString('Authentication', LTokenName + 'Email', jwt.Payload.GetValue('email'));
    SendAddress := jwt.Payload.GetValue('email');
  end;

  DoLog('Authenticated via OAUTH2');
  DoLog(FOAuth2_Enhanced.RefreshToken);
  AResponseInfo.ContentText := '<html><body>Successfully Authenticated. You can now close this tab/window.</body></html>';
  ForceForegroundNoActivate(HWNDHandle);
end;

function TEmailOAuthDataModule.IsAuthenticated: boolean;
begin
  Result := FIsAuthenticated;
end;

function TEmailOAuthDataModule.ReadString(const Ident, Default: string): string;
begin
  Result := FIniSettings.ReadString('Authentication', Ident, '');
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
begin
  // Delete persistent Refresh_token.  Note
  //  - This probably should have a logout function called on it
  //  - The token should be stored in an encrypted way ... but this is just a demo.
  FIniSettings.DeleteKey('Authentication', Provider.TokenName);
  SetupAuthenticator;
end;

function TEmailOAuthDataModule.SendEmailUsingREST(const fromAddress: string;
                                const fromName: string;
                                const recepientAddress: string;
                                const recepientName: string;
                                const subject: string;
                                const body: string;
                                const Path: string): Boolean;
var
  JSONBody: TJSONObject;
  MessageObj, BodyObj, FromObj, RecipientObj, EmailAddrObj: TJSONObject;
  ToRecipientsArr: TJSONArray;
begin
  JSONBody := nil;
  MessageObj := nil;
  BodyObj := nil;
  FromObj := nil;
  RecipientObj := nil;
  EmailAddrObj := nil;
  ToRecipientsArr := nil;
  RESTRequestGraph.ResetToDefaults;
  FOAuth2_Enhanced.RefreshAccessTokenIfRequired;
  RESTRequestGraph.Client.Authenticator := FOAuth2_Enhanced;
  try
    MessageObj := TJSONObject.Create;
    BodyObj := TJSONObject.Create;
    FromObj := TJSONObject.Create;
    RecipientObj := TJSONObject.Create;
    EmailAddrObj := TJSONObject.Create;
    ToRecipientsArr := TJSONArray.Create;

    BodyObj.AddPair('contentType', 'HTML');
    BodyObj.AddPair('content', Body);

    EmailAddrObj.AddPair('address', fromAddress);
    EmailAddrObj.AddPair('name', fromName);

    FromObj.AddPair('emailAddress', EmailAddrObj);

    RecipientObj.AddPair('emailAddress', TJSONObject.Create.AddPair('address', recepientAddress));

    ToRecipientsArr.Add(RecipientObj);
    MessageObj.AddPair('subject', Subject);
    MessageObj.AddPair('body', BodyObj);
    MessageObj.AddPair('from', FromObj);
    MessageObj.AddPair('toRecipients', ToRecipientsArr);
    JSONBody := TJSONObject.Create;
    JSONBody.AddPair('message', MessageObj);
    RESTRequestGraph.Resource := 'me/sendMail';
    RESTRequestGraph.Method := TRESTRequestMethod.rmPOST;
    RESTRequestGraph.AddBody(JSONBody.ToString, TRESTContentType.ctAPPLICATION_JSON);
    try
      RESTRequestGraph.Execute;
      Result := RESTResponseGraph.StatusCode = 202;
    except
      on E: Exception do
      begin
        ShowMessage('Error: ' + E.Message);
        Result := False;
      end;
    end;
  finally
    FreeAndNil(JSONBody);
  end;
end;

procedure TEmailOAuthDataModule.SendMessage(const fromAddress: string; const fromName:string;
                const recepientAddress: string; const recepientName: string; const subject: string;
                const body: string;
                const Path: String);
var
  IdMessage: TIdMessage;
  xoauthSASL : TIdSASLListEntry;
  oldRefreshToken : string;
  LTokenName : string;
  recepient: TIdEMailAddressItem;
begin
  // if we only have refresh_token or access token has expired
  // request new access_token to use with request
  FOAuth2_Enhanced.ClientID := Provider.ClientID;
  FOAuth2_Enhanced.ClientSecret := Provider.ClientSecret;

  FOAuth2_Enhanced.RefreshAccessTokenIfRequired;
  if (oldRefreshToken <> FOAuth2_Enhanced.RefreshToken) and (not FOAuth2_Enhanced.RefreshToken.IsEmpty) then
  begin
    LTokenName := Provider.AuthName + 'Token';
    FIniSettings.WriteString('Authentication', LTokenName, FOAuth2_Enhanced.RefreshToken);
  end;

  DoLog('refresh_token=' + FOAuth2_Enhanced.RefreshToken);
  DoLog('access_token=' + FOAuth2_Enhanced.AccessToken);

  if FOAuth2_Enhanced.AccessToken.Length = 0 then
  begin
    DoLog('Failed to authenticate properly');
    Exit;
  end;

  IdSMTP1.Host := Provider.SmtpHost;
  IdSMTP1.UseTLS := Provider.TLS;
  IdSSLIOHandlerSocketSMTP.SSLOptions.MinTLSVersion := Provider.Version;
  IdSMTP1.Port := Provider.SmtpPort;

  xoauthSASL := IdSMTP1.SASLMechanisms.Add;
  xoauthSASL.SASL := Provider.AuthenticationType.Create(nil);

  TIdSASLOAuthBase(xoauthSASL.SASL).Token := FOAuth2_Enhanced.AccessToken;
  TIdSASLOAuthBase(xoauthSASL.SASL).User := fromAddress;

  IdSMTP1.Connect;

  IdSMTP1.AuthType := satSASL;
  IdSMTP1.Authenticate;

  IdMessage := TIdMessage.Create(Self);
  IdMessage.From.Address := fromAddress;
  IdMessage.From.Name := fromName;
  IdMessage.ReplyTo.EMailAddresses := IdMessage.From.Address;
  recepient := IdMessage.Recipients.Add;
  recepient.Name := recepientName;
  recepient.Address := recepientAddress;
  IdMessage.Subject := subject;
  IdMessage.Body.Text := body;

  IdSMTP1.Send(IdMessage);
  IdSMTP1.Disconnect;
  ShowMessage('Message Sent');
end;

procedure TEmailOAuthDataModule.SendEmailWithAttachment(const fromAddress: string; const fromName:string;
                const recepientAddress: string; const recepientName: string; const subjectText: string;
                const PlainBody, HtmlBody: string;
                const InlineImagePaths: array of string; const AttachmentPath: array of string);
var
  xoauthSASL : TIdSASLListEntry;
  Email: TIdMessage;
  recepient: TIdEMailAddressItem;
  PlainTextPart, HTMLPart: TIdText;
  AlternativePart: TIdMessageParts;
  RelatedPart: TIdMessageParts;
  ImagePart: TIdAttachmentFile;
  FileAttachment: TIdAttachmentFile;
  AltParentIdx, RelatedParentIdx: Integer;
  ContentIdList: TArray<string>;
  i: Integer;
  oldRefreshToken : string;
  LTokenName : string;
begin
  Email := nil;
  try
    Email := TIdMessage.Create(Self);
    Email.Clear;
    Email.ContentType := 'multipart/mixed';  // OUTERMOST is mixed
    Email.CharSet := 'utf-8';
    Email.Subject := subjectText;
    Email.From.Address := fromAddress;
    Email.From.Name := fromName;
    Email.ReplyTo.EMailAddresses := Email.From.Address;

    recepient := Email.Recipients.Add;
    recepient.Name := recepientName;
    recepient.Address := recepientAddress;

    // STEP 1: Create multipart/alternative
    AltParentIdx := Email.MessageParts.Count; // will point to this "multipart/alternative" header
    with TIdText.Create(Email.MessageParts, nil) do
    begin
      ContentType := 'multipart/alternative';
    end;

    // STEP 2: Plain text part (child of AltParentIdx)
    PlainTextPart := TIdText.Create(Email.MessageParts, nil);
    PlainTextPart.ParentPart := AltParentIdx;
    PlainTextPart.ContentType := 'text/plain; charset=UTF-8';
    PlainTextPart.Body.Text := PlainBody;

    // STEP 3: Create multipart/related inside alternative
    RelatedParentIdx := Email.MessageParts.Count;
    with TIdText.Create(Email.MessageParts, nil) do
    begin
      ParentPart := AltParentIdx;
      ContentType := 'multipart/related';
    end;

    // STEP 4: HTML part (child of RelatedParentIdx)
    HtmlPart := TIdText.Create(Email.MessageParts, nil);
    HtmlPart.ParentPart := RelatedParentIdx;
    HtmlPart.ContentType := 'text/html; charset=UTF-8';
    HtmlPart.Body.Text := HtmlBody;

    // STEP 5: Inline images (child of RelatedParentIdx)
    SetLength(ContentIdList, Length(InlineImagePaths));
    for i := Low(InlineImagePaths) to High(InlineImagePaths) do
    begin
      ContentIdList[i] := Format('image%d@domain.com', [i]);
      ImagePart := TIdAttachmentFile.Create(Email.MessageParts, InlineImagePaths[i]);
      ImagePart.ParentPart := RelatedParentIdx;
      ImagePart.ContentID := ContentIdList[i];
      ImagePart.ContentDisposition := 'inline';
    end;

    // Fix HTML img tags
    for i := Low(ContentIdList) to High(ContentIdList) do
    begin
      HtmlPart.Body.Text := StringReplace(HtmlPart.Body.Text,
        Format('{img%d}', [i]),
        Format('cid:%s', [ContentIdList[i]]),
        [rfReplaceAll]);
    end;

    // STEP 6: Attachments (top-level, no parent)
    for i := Low(AttachmentPath) to High(AttachmentPath) do
    if AttachmentPath[i].Length > 0 then
    begin
      FileAttachment := TIdAttachmentFile.Create(Email.MessageParts, AttachmentPath[i]);
      FileAttachment.ContentDisposition := 'attachment';
    end;

    // Save to disk for testing
    // Email.SaveToFile('C:\Programming\GmailAuthSMTP\email.txt');

    // Configure SMTP
    // if we only have refresh_token or access token has expired
    // request new access_token to use with request
    FOAuth2_Enhanced.ClientID := Provider.ClientID;
    FOAuth2_Enhanced.ClientSecret := Provider.ClientSecret;
    FOAuth2_Enhanced.RefreshAccessTokenIfRequired;
    if (oldRefreshToken <> FOAuth2_Enhanced.RefreshToken) and (not FOAuth2_Enhanced.RefreshToken.IsEmpty) then
    begin
      LTokenName := Provider.AuthName + 'Token';
      FIniSettings.WriteString('Authentication', LTokenName, FOAuth2_Enhanced.RefreshToken);
    end;
    DoLog('refresh_token=' + FOAuth2_Enhanced.RefreshToken);
    DoLog('access_token=' + FOAuth2_Enhanced.AccessToken);
    if FOAuth2_Enhanced.AccessToken.Length = 0 then
    begin
      DoLog('Failed to authenticate properly');
      Exit;
    end;

    // if we only have refresh_token or access token has expired
    // request new access_token to use with request
    FOAuth2_Enhanced.ClientID := Provider.ClientID;
    FOAuth2_Enhanced.ClientSecret := Provider.ClientSecret;

    FOAuth2_Enhanced.RefreshAccessTokenIfRequired;
    if (oldRefreshToken <> FOAuth2_Enhanced.RefreshToken) and (not FOAuth2_Enhanced.RefreshToken.IsEmpty) then
    begin
      LTokenName := Provider.AuthName + 'Token';
      FIniSettings.WriteString('Authentication', LTokenName, FOAuth2_Enhanced.RefreshToken);
    end;

    DoLog('refresh_token=' + FOAuth2_Enhanced.RefreshToken);
    DoLog('access_token=' + FOAuth2_Enhanced.AccessToken);

    if FOAuth2_Enhanced.AccessToken.Length = 0 then
    begin
      DoLog('Failed to authenticate properly');
      Exit;
    end;

    IdSMTP1.Host := Provider.SmtpHost;
    IdSMTP1.UseTLS := Provider.TLS;
    IdSSLIOHandlerSocketSMTP.SSLOptions.MinTLSVersion := Provider.Version;
    IdSMTP1.Port := Provider.SmtpPort;

    xoauthSASL := IdSMTP1.SASLMechanisms.Add;
    xoauthSASL.SASL := Provider.AuthenticationType.Create(nil);

    TIdSASLOAuthBase(xoauthSASL.SASL).Token := FOAuth2_Enhanced.AccessToken;
    TIdSASLOAuthBase(xoauthSASL.SASL).User := fromAddress;

    IdSMTP1.Connect;

    IdSMTP1.AuthType := satSASL;
    IdSMTP1.Authenticate;
    try
      IdSMTP1.Send(Email);
    finally
      IdSMTP1.Disconnect;
    end;
  finally
    FreeAndNil(Email);
  end;
end;

procedure TEmailOAuthDataModule.CheckIMAP;
var
  xoauthSASL : TIdSASLListEntry;
  msgCount : Integer;
  mailboxes : TStringList;
  oldRefreshToken : string;
  LTokenName : string;
begin
  DoLog('refresh_token=' + FOAuth2_Enhanced.RefreshToken);
  DoLog('access_token=' + FOAuth2_Enhanced.AccessToken);

  // if we only have refresh_token or access token has expired
  // request new access_token to use with request
  FOAuth2_Enhanced.ClientID := Provider.ClientID;
  FOAuth2_Enhanced.ClientSecret := Provider.ClientSecret;
  oldRefreshToken := FOAuth2_Enhanced.RefreshToken;
  FOAuth2_Enhanced.RefreshAccessTokenIfRequired;
  if (oldRefreshToken <> FOAuth2_Enhanced.RefreshToken) and (not FOAuth2_Enhanced.RefreshToken.IsEmpty) then
  begin
    LTokenName := Provider.AuthName + 'Token';
    FIniSettings.WriteString('Authentication', LTokenName, FOAuth2_Enhanced.RefreshToken);
  end;

  if FOAuth2_Enhanced.AccessToken.Length = 0 then
  begin
    DoLog('Failed to authenticate properly');
    Exit;
  end;

  IdIMAP.Host := Provider.ImapHost;
  IdIMAP.Port := Provider.ImapPort;
  IdIMAP.UseTLS := Provider.TLS;
  IdSSLIOHandlerSocketIMAP.SSLOptions.MinTLSVersion := Provider.Version;

  xoauthSASL := IdIMAP.SASLMechanisms.Add;
  xoauthSASL.SASL := Provider.AuthenticationType.Create(nil);

  TIdSASLOAuthBase(xoauthSASL.SASL).Token := FOAuth2_Enhanced.AccessToken;
  TIdSASLOAuthBase(xoauthSASL.SASL).User := SendAddress;

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
  IdSSLIOHandlerSocketPOP.SSLOptions.MinTLSVersion := Provider.Version;

  xoauthSASL := IdPOP3.SASLMechanisms.Add;
  xoauthSASL.SASL := Provider.AuthenticationType.Create(nil);

  TIdSASLOAuthBase(xoauthSASL.SASL).Token := FOAuth2_Enhanced.AccessToken;
  TIdSASLOAuthBase(xoauthSASL.SASL).User := SendAddress;

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
begin
  FOAuth2_Enhanced.ClientID := Provider.ClientID;
  FOAuth2_Enhanced.ClientSecret := Provider.Clientsecret;
  FOAuth2_Enhanced.Scope := Provider.Scopes;
  FOAuth2_Enhanced.RedirectionEndpoint := clientredirect;
  FOAuth2_Enhanced.AuthorizationEndpoint := Provider.AuthorizationEndpoint;
  FOAuth2_Enhanced.AccessTokenEndpoint := Provider.AccessTokenEndpoint;

  FOAuth2_Enhanced.RefreshToken := FIniSettings.ReadString('Authentication', Provider.TokenName, '');
  SendAddress := FIniSettings.ReadString('Authentication', Provider.TokenName + 'Email', '');


  FOAuth2_Enhanced.AccessToken := '';
  FOAuth2_Enhanced.AccessTokenExpiry := 0;
  IdSMTP1.Disconnect;
  IdPOP3.Disconnect;
  IdIMAP.Disconnect;
end;

procedure TEmailOAuthDataModule.WriteString(const Ident, Value: String);
begin
  FIniSettings.WriteString('Authentication', Ident, Value);
end;

end.
