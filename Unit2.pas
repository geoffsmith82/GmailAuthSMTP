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
  Vcl.ExtCtrls,
  EmailOAuthDm,
  Globals
  ;

type
  TForm2 = class(TForm)
    btnAuthenticate: TButton;
    btnSendMsg: TButton;
    rgEmailProviders: TRadioGroup;
    btnCheckMsg: TButton;
    btnClearAuthToken: TButton;
    btnCheckIMAP: TButton;
    Memo1: TMemo;
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnCheckMsgClick(Sender: TObject);
    procedure btnClearAuthTokenClick(Sender: TObject);
    procedure rgEmailProvidersClick(Sender: TObject);
    procedure btnCheckIMAPClick(Sender: TObject);
    procedure btnAuthenticateClick(Sender: TObject);
    procedure btnSendMsgClick(Sender: TObject);
  private
    { Private declarations }
    EmailOAuthDataModule : TEmailOAuthDataModule;
    procedure LogMsg(msg: string);
  public
    { Public declarations }
    procedure UpdateButtonsEnabled;
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

procedure TForm2.FormDestroy(Sender: TObject);
begin

  FreeAndNil(EmailOAuthDataModule);
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  EmailOAuthDataModule := TEmailOAuthDataModule.Create(nil);
  EmailOAuthDataModule.OnLog := LogMsg;
  EmailOAuthDataModule.SetupAuthenticator;
  UpdateButtonsEnabled;
end;


procedure TForm2.UpdateButtonsEnabled;
begin
  btnAuthenticate.Enabled :=  not EmailOAuthDataModule.IsAuthenticated;
  btnClearAuthToken.Enabled :=  EmailOAuthDataModule.IsAuthenticated;
end;

procedure TForm2.btnAuthenticateClick(Sender: TObject);
begin
  EmailOAuthDataModule.Authenticate;
  UpdateButtonsEnabled;
end;

procedure TForm2.btnCheckIMAPClick(Sender: TObject);
begin
  EmailOAuthDataModule.CheckIMAP;
end;

procedure TForm2.btnCheckMsgClick(Sender: TObject);
begin
  EmailOAuthDataModule.CheckPOP;
end;

procedure TForm2.btnClearAuthTokenClick(Sender: TObject);
begin
  EmailOAuthDataModule.ClearAuthentication;
end;

procedure TForm2.btnSendMsgClick(Sender: TObject);
begin
  EmailOAuthDataModule.SendMessage('');
end;

procedure TForm2.LogMsg(msg: string);
begin
  Memo1.Lines.Add(msg);
end;

procedure TForm2.rgEmailProvidersClick(Sender: TObject);
begin
  EmailOAuthDataModule.SelectedProvider := rgEmailProviders.ItemIndex;
  EmailOAuthDataModule.SetupAuthenticator;
end;



end.
