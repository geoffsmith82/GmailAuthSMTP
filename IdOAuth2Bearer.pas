unit IdOAuth2Bearer;

interface

uses
  Classes,
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
  VInitialResponse := 'n,a=' + FUser + ',' + Chr($01) + 'host=' + FHost + Chr($01) + 'port=465' + Chr($01) + 'auth=Bearer ' + FToken + Chr($01) + Chr($01);
  Result := True;
end;

end.
