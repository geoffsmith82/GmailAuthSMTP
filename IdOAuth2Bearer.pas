unit IdOAuth2Bearer;

interface

uses
  System.Classes,
  System.SysUtils,
  IdSASL
  ;

type
  TIdOAuth2Bearer = class(TIdSASL)
  private
    FToken: string;
    FHost: string;
    FUser: string;
    FPort: Integer;
  public
    property Token: string read FToken write FToken;
    property Host: string read FHost write FHost;
    property Port: Integer read FPort write FPort;
    property User: string read FUser write FUser;
    class function ServiceName: TIdSASLServiceName; override;
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    function TryStartAuthenticate(const AHost, AProtocolName : String; var VInitialResponse: String): Boolean; override;
    function ContinueAuthenticate(const ALastResponse, AHost, AProtocolName : string): string; override;
    function StartAuthenticate(const AChallenge, AHost, AProtocolName: string): string; override;
    { For cleaning up after Authentication }
    procedure FinishAuthenticate; override;
  end;

implementation

{ TIdOAuth2Bearer }

constructor TIdOAuth2Bearer.Create(AOwner: TComponent);
begin
  inherited;
end;

destructor TIdOAuth2Bearer.Destroy;
begin
  inherited;
end;

class function TIdOAuth2Bearer.ServiceName: TIdSASLServiceName;
begin
  Result := 'OAUTHBEARER';
end;

function TIdOAuth2Bearer.TryStartAuthenticate(const AHost, AProtocolName: String; var VInitialResponse: String): Boolean;
begin
  VInitialResponse := 'n,a=' + FUser + ',' + Chr($01) + 'host=' + FHost + Chr($01) + 'port=' + FPort.ToString + Chr($01) + 'auth=Bearer ' + FToken + Chr($01) + Chr($01);
  Result := True;
end;

function TIdOAuth2Bearer.StartAuthenticate(const AChallenge, AHost, AProtocolName: string): string;
begin
  Result := 'n,a=' + FUser + ',' + Chr($01) + 'host=' + FHost + Chr($01) + 'port=' + FPort.ToString + Chr($01) + 'auth=Bearer ' + FToken + Chr($01) + Chr($01);
end;

function TIdOAuth2Bearer.ContinueAuthenticate(const ALastResponse, AHost, AProtocolName: string): string;
begin
  // Nothing to do
end;

procedure TIdOAuth2Bearer.FinishAuthenticate;
begin
  // Nothing to do
end;

end.
