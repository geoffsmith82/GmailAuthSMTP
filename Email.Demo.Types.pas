unit Email.Demo.Types;

interface

uses
    IdSASL.OAuth.Base
  , IdExplicitTLSClientServerBase
  ;

type

  TAuthType = class of TIdSASLOAuthBase;

  TOnLog = procedure(const log: string) of object;

  TProviderInfo = record
    AuthenticationType : TAuthType;
    AuthorizationEndpoint : string;
    AccessTokenEndpoint : string;
    LogoutEndpoint : string;
    ClientID : String;
    ClientSecret : string;
    ClientAccount : string;
    ClientName : string;
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
    function TokenName: string;
  end;


implementation

{ TProviderInfo }

function TProviderInfo.TokenName: string;
begin
  Result := AuthName + 'Token';
end;

end.
