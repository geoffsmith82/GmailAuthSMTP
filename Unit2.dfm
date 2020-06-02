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
  OnDestroy = FormDestroy
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
  object btnAuthenticate: TButton
    Left = 423
    Top = 8
    Width = 106
    Height = 25
    Caption = 'Authenticate'
    TabOrder = 1
    OnClick = btnAuthenticateClick
  end
  object btnSendMsg: TButton
    Left = 423
    Top = 95
    Width = 75
    Height = 25
    Caption = 'Send MSG'
    TabOrder = 2
    OnClick = btnSendMsgClick
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
  object btnCheckMsg: TButton
    Left = 423
    Top = 136
    Width = 75
    Height = 25
    Caption = 'Check MSG'#39's'
    TabOrder = 4
    OnClick = btnCheckMsgClick
  end
  object btnClearAuthToken: TButton
    Left = 423
    Top = 39
    Width = 106
    Height = 25
    Caption = 'Clear Auth Token'
    TabOrder = 5
    OnClick = btnClearAuthTokenClick
  end
  object IdSMTP1: TIdSMTP
    IOHandler = IdSSLIOHandlerSocketSMTP
    SASLMechanisms = <>
    Left = 88
    Top = 128
  end
  object IdSSLIOHandlerSocketSMTP: TIdSSLIOHandlerSocketOpenSSL
    Destination = ':25'
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
  object IdConnectionInterceptSMTP: TIdConnectionIntercept
    OnReceive = IdConnectionInterceptSMTPReceive
    OnSend = IdConnectionInterceptSMTPSend
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
  object IdPOP3: TIdPOP3
    Intercept = IdConnectionPOP
    IOHandler = IdSSLIOHandlerSocketPOP
    AuthType = patSASL
    AutoLogin = False
    SASLMechanisms = <>
    Left = 456
    Top = 232
  end
  object IdConnectionPOP: TIdConnectionIntercept
    OnReceive = IdConnectionInterceptSMTPReceive
    OnSend = IdConnectionInterceptSMTPSend
    Left = 96
    Top = 272
  end
  object IdSSLIOHandlerSocketPOP: TIdSSLIOHandlerSocketOpenSSL
    Destination = ':110'
    Intercept = IdConnectionPOP
    MaxLineAction = maException
    Port = 110
    DefaultPort = 0
    SSLOptions.Method = sslvTLSv1_2
    SSLOptions.SSLVersions = [sslvTLSv1_2]
    SSLOptions.Mode = sslmClient
    SSLOptions.VerifyMode = []
    SSLOptions.VerifyDepth = 0
    Left = 256
    Top = 264
  end
end
