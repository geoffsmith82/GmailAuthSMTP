program GMailAuthDemo;

uses
  Vcl.Forms,
  Unit2 in 'Unit2.pas' {Form2},
  Globals in 'Globals.pas',
  IdSASL.OAuth.XOAUTH2 in 'IdSASL.OAuth.XOAUTH2.pas',
  EmailOAuthDm in 'EmailOAuthDm.pas' {EmailOAuthDataModule: TDataModule},
  IdSASL.OAuth.OAuth2Bearer in 'IdSASL.OAuth.OAuth2Bearer.pas',
  REST.Authenticator.EnhancedOAuth in 'REST.Authenticator.EnhancedOAuth.pas',
  Email.Demo.Types in 'Email.Demo.Types.pas',
  IdSASL.OAuth.Base in 'IdSASL.OAuth.Base.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
