unit IdSASL.OAuth.XOAUTH2;

interface

uses
  Classes
  , SysUtils
  , IdSASL
  , IdSASL.OAuth.Base
  ;

type
  TIdSASLXOAuth = class(TIdSASLOAuthBase)
  public
    class function ServiceName: TIdSASLServiceName; override;
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    function TryStartAuthenticate(const AHost, AProtocolName : string; var VInitialResponse: string): Boolean; override;
    function ContinueAuthenticate(const ALastResponse, AHost, AProtocolName : string): string; override;
    function StartAuthenticate(const AChallenge, AHost, AProtocolName: string): string; override;
    { For cleaning up after Authentication }
    procedure FinishAuthenticate; override;
  end;

implementation

{ TIdSASLXOAuth }

class function TIdSASLXOAuth.ServiceName: TIdSASLServiceName;
begin
  Result := 'XOAUTH2';
end;

constructor TIdSASLXOAuth.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TIdSASLXOAuth.Destroy;
begin
  inherited;
end;

function TIdSASLXOAuth.TryStartAuthenticate(const AHost, AProtocolName : string; var VInitialResponse: string): Boolean;
begin
    VInitialResponse := Format('user=%s'#1'auth=Bearer %s'#1#1, [FUser, FToken]); {do not localize}
    Result := True;
end;

function TIdSASLXOAuth.StartAuthenticate(const AChallenge, AHost, AProtocolName: string): string;
begin
    Result := Format('user=%s'#1'auth=Bearer %s'#1#1, [FUser, FToken]); {do not localize}
end;

function TIdSASLXOAuth.ContinueAuthenticate(const ALastResponse, AHost, AProtocolName: string): string;
begin
  // Nothing to do
end;

procedure TIdSASLXOAuth.FinishAuthenticate;
begin
  // Nothing to do
end;

end.

