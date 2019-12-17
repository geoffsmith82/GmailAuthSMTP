object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'Test OAUTH2 Gmail Send Message'
  ClientHeight = 377
  ClientWidth = 594
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Memo1: TMemo
    Left = 8
    Top = 72
    Width = 409
    Height = 182
    Lines.Strings = (
      'Memo1')
    TabOrder = 0
  end
  object Button1: TButton
    Left = 423
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Authenticate'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 423
    Top = 39
    Width = 75
    Height = 25
    Caption = 'Send MSG'
    TabOrder = 2
    OnClick = Button2Click
  end
  object rgEmailProviders: TRadioGroup
    Left = 8
    Top = 8
    Width = 409
    Height = 58
    Caption = 'Provider'
    Columns = 2
    ItemIndex = 0
    Items.Strings = (
      'GMail'
      'Microsoft')
    TabOrder = 3
    OnClick = rgEmailProvidersClick
  end
  object IdSMTP1: TIdSMTP
    Intercept = IdConnectionIntercept1
    IOHandler = IdSSLIOHandlerSocketOpenSSL1
    SASLMechanisms = <>
    Left = 88
    Top = 128
  end
  object IdSSLIOHandlerSocketOpenSSL1: TIdSSLIOHandlerSocketOpenSSL
    Destination = ':25'
    Intercept = IdConnectionIntercept1
    MaxLineAction = maException
    Port = 25
    DefaultPort = 0
    SSLOptions.Method = sslvSSLv23
    SSLOptions.SSLVersions = [sslvTLSv1_1, sslvTLSv1_2]
    SSLOptions.Mode = sslmUnassigned
    SSLOptions.VerifyMode = []
    SSLOptions.VerifyDepth = 0
    Left = 248
    Top = 56
  end
  object IdConnectionIntercept1: TIdConnectionIntercept
    OnReceive = IdConnectionIntercept1Receive
    OnSend = IdConnectionIntercept1Send
    Left = 88
    Top = 64
  end
  object IdHTTPServer1: TIdHTTPServer
    Active = True
    Bindings = <>
    DefaultPort = 2132
    OnCommandGet = IdHTTPServer1CommandGet
    Left = 352
    Top = 120
  end
end
