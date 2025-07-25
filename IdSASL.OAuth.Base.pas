unit IdSASL.OAuth.Base;

interface

uses
    Classes
  , SysUtils
  , IdSASL
  ;

type
  TIdSASLOAuthBase = class(TIdSASL)
  protected
    FToken: string;
    FUser: string;
  public
    property User: string read FUser write FUser;
    property Token: string read FToken write FToken;
  end;

  TIdSASLOAuthBaseClass = class of TIdSASLOAuthBase;

implementation

end.
