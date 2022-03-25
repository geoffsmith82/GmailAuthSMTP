object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'Test OAUTH2 Gmail Send Message'
  ClientHeight = 943
  ClientWidth = 1485
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -28
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 240
  TextHeight = 34
  object btnAuthenticate: TButton
    Left = 1059
    Top = 19
    Width = 265
    Height = 63
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Caption = 'Authenticate'
    TabOrder = 0
    OnClick = btnAuthenticateClick
  end
  object btnSendMsg: TButton
    Left = 1058
    Top = 238
    Width = 187
    Height = 62
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Caption = 'Send MSG'
    TabOrder = 1
    OnClick = btnSendMsgClick
  end
  object rgEmailProviders: TRadioGroup
    Left = 20
    Top = 20
    Width = 1023
    Height = 145
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Caption = 'Provider'
    Columns = 3
    ItemIndex = 0
    Items.Strings = (
      'GMail'
      'Microsoft'
      'Hotmail')
    TabOrder = 2
    OnClick = rgEmailProvidersClick
  end
  object btnCheckMsg: TButton
    Left = 1058
    Top = 340
    Width = 187
    Height = 63
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Caption = 'Check MSG'#39's'
    TabOrder = 3
    OnClick = btnCheckMsgClick
  end
  object btnClearAuthToken: TButton
    Left = 1058
    Top = 98
    Width = 265
    Height = 62
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Caption = 'Clear Auth Token'
    TabOrder = 4
    OnClick = btnClearAuthTokenClick
  end
  object btnCheckIMAP: TButton
    Left = 1060
    Top = 420
    Width = 188
    Height = 63
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Caption = 'Check IMAP'
    TabOrder = 5
    OnClick = btnCheckIMAPClick
  end
  object Memo1: TMemo
    Left = 20
    Top = 180
    Width = 1023
    Height = 455
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Lines.Strings = (
      'Memo1')
    TabOrder = 6
  end
end
