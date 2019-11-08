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
  REST.Authenticator.OAuth, IdContext
  ;

type
  TForm2 = class(TForm)
    IdSMTP1: TIdSMTP;
    IdSSLIOHandlerSocketOpenSSL1: TIdSSLIOHandlerSocketOpenSSL;
    Memo1: TMemo;
    IdConnectionIntercept1: TIdConnectionIntercept;
    OAuth2_GMail: TOAuth2Authenticator;
    Button1: TButton;
    Button2: TButton;
    IdHTTPServer1: TIdHTTPServer;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure IdConnectionIntercept1Receive(ASender: TIdConnectionIntercept; var ABuffer: TIdBytes);
    procedure IdHTTPServer1CommandGet(AContext: TIdContext; ARequestInfo:
        TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

uses
  System.NetEncoding,
  REST.Utils,
  Winapi.ShellAPI,
  IdMessage,
  GmailGlobals
  ;

procedure TForm2.FormCreate(Sender: TObject);
begin
  OAuth2_GMail.ClientID := clientid;
  OAuth2_GMail.ClientSecret := clientsecret;
  OAuth2_GMail.Scope := clientscope;
  OAuth2_GMail.RedirectionEndpoint := clientredirect;
end;

procedure TForm2.Button1Click(Sender: TObject);
begin
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
  XOAUTH2String : String;
  base64 : TBase64Encoding;
  IdMessage: TIdMessage;
begin
  IdSMTP1.AuthType := satNone;

  base64 := TBase64Encoding.Create(0);
  try
    OAuth2_GMail.ChangeAuthCodeToAccesToken;

    XOAUTH2String := base64.Encode('user=' + clientaccount + Chr($01) + 'auth=Bearer ' + OAuth2_GMail.AccessToken + Chr($01) + Chr($01));

    IdSMTP1.Host := 'smtp.gmail.com';
    IdSMTP1.Port := 465;
    IdSMTP1.UseTLS := utUseImplicitTLS;
    IdSMTP1.Connect;
    Memo1.Lines.Add(XOAUTH2String);

    IdSMTP1.SendCmd('AUTH XOAUTH2 ' + XOAUTH2String);

    IdMessage := TIdMessage.Create(Self);
    IdMessage.From.Address := clientaccount;
    IdMessage.From.Name := clientname;
    IdMessage.ReplyTo.EMailAddresses := IdMessage.From.Address;
    IdMessage.Recipients.Add.Text := clientsendtoaddress;
    IdMessage.Subject := 'Hello World';
    IdMessage.Body.Text := 'Hello Body';

    IdSMTP1.Send(IdMessage);
  finally
    FreeAndNil(base64);
  end;
end;

procedure TForm2.IdConnectionIntercept1Receive(ASender: TIdConnectionIntercept; var ABuffer: TIdBytes);
begin
  Memo1.Lines.Add(TEncoding.ASCII.GetString(ABuffer));
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
    end;
  end;
end;

end.
