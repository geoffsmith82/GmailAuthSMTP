object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'Test OAUTH2 Gmail Send Message'
  ClientHeight = 300
  ClientWidth = 594
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 13
  object btnAuthenticate: TButton
    Left = 424
    Top = 8
    Width = 106
    Height = 25
    Caption = 'Authenticate'
    TabOrder = 0
    OnClick = btnAuthenticateClick
  end
  object btnSendMsg: TButton
    Left = 423
    Top = 95
    Width = 75
    Height = 25
    Caption = 'Send MSG'
    TabOrder = 1
    OnClick = btnSendMsgClick
  end
  object rgEmailProviders: TRadioGroup
    Left = 8
    Top = 8
    Width = 409
    Height = 58
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
    Left = 423
    Top = 136
    Width = 75
    Height = 25
    Caption = 'Check MSG'#39's'
    TabOrder = 3
    OnClick = btnCheckMsgClick
  end
  object btnClearAuthToken: TButton
    Left = 423
    Top = 39
    Width = 106
    Height = 25
    Caption = 'Clear Auth Token'
    TabOrder = 4
    OnClick = btnClearAuthTokenClick
  end
  object btnCheckIMAP: TButton
    Left = 424
    Top = 168
    Width = 75
    Height = 25
    Caption = 'Check IMAP'
    TabOrder = 5
    OnClick = btnCheckIMAPClick
  end
  object Memo1: TMemo
    Left = 8
    Top = 72
    Width = 409
    Height = 182
    Lines.Strings = (
      'Memo1')
    TabOrder = 6
  end
end
