unit Email.Demo.Types;

interface

uses
    IdSASL
  , IdSASLCollection
  , IdExplicitTLSClientServerBase
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


implementation

end.
