unit IdSASLXOAUTH;

interface

uses
  Classes,
  IdSASL
  ;

type
  TIdSASLXOAuth = class(TIdSASL)
  private
    FToken: string;
    FUser: string;
    FTwoLinePopFormat: Boolean;
  public
    property Token: string read FToken write FToken;
    property User: string read FUser write FUser;
    property TwoLinePopFormat: Boolean read FTwoLinePopFormat write FTwoLinePopFormat;
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

{ TIdSASLXOAuth }

class function TIdSASLXOAuth.ServiceName: TIdSASLServiceName;
begin
  Result := 'XOAUTH2';
end;

constructor TIdSASLXOAuth.Create(AOwner: TComponent);
begin
  inherited;
end;

destructor TIdSASLXOAuth.Destroy;
begin
  inherited;
end;

function TIdSASLXOAuth.TryStartAuthenticate(const AHost, AProtocolName: String; var VInitialResponse: String): Boolean;
begin
  if (AProtocolName = 'pop') and FTwoLinePopFormat then
  begin
    // Don't send anything yet
  end
  else
  begin
    VInitialResponse := 'user=' + FUser + Chr($01) + 'auth=Bearer ' + FToken + Chr($01) + Chr($01);
  end;
  Result := True;
end;

function TIdSASLXOAuth.StartAuthenticate(const AChallenge, AHost, AProtocolName: string): string;
begin
  Result := 'user=' + FUser + Chr($01) + 'auth=Bearer ' + FToken + Chr($01) + Chr($01);
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

