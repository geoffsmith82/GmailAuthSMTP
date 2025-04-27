object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'Test OAUTH2 Gmail Send Message'
  ClientHeight = 943
  ClientWidth = 1518
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -28
  Font.Name = 'Tahoma'
  Font.Style = []
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 240
  DesignSize = (
    1518
    943)
  TextHeight = 34
  object btnAuthenticate: TButton
    Left = 1200
    Top = 19
    Width = 321
    Height = 63
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Anchors = [akTop, akRight]
    Caption = 'Authenticate'
    TabOrder = 0
    OnClick = btnAuthenticateClick
  end
  object btnSendMsg: TButton
    Left = 1200
    Top = 218
    Width = 314
    Height = 62
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Anchors = [akTop, akRight]
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
    Left = 1200
    Top = 524
    Width = 314
    Height = 63
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Anchors = [akTop, akRight]
    Caption = 'Check MSG'#39's'
    TabOrder = 3
    OnClick = btnCheckMsgClick
  end
  object btnClearAuthToken: TButton
    Left = 1200
    Top = 98
    Width = 314
    Height = 62
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Anchors = [akTop, akRight]
    Caption = 'Clear Auth Token'
    TabOrder = 4
    OnClick = btnClearAuthTokenClick
  end
  object btnCheckIMAP: TButton
    Left = 1200
    Top = 624
    Width = 314
    Height = 63
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Anchors = [akTop, akRight]
    Caption = 'Check IMAP'
    TabOrder = 5
    OnClick = btnCheckIMAPClick
  end
  object btnSendViaREST: TButton
    Left = 1200
    Top = 402
    Width = 314
    Height = 62
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Anchors = [akTop, akRight]
    Caption = 'Send MSG via REST'
    TabOrder = 6
    OnClick = btnSendViaRESTClick
  end
  object PageControl1: TPageControl
    Left = 20
    Top = 181
    Width = 1141
    Height = 749
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    ActivePage = tsEmail
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 7
    object tsEmail: TTabSheet
      Margins.Left = 8
      Margins.Top = 8
      Margins.Right = 8
      Margins.Bottom = 8
      Caption = 'Email'
      DesignSize = (
        1121
        682)
      object lblFrom: TLabel
        Left = 88
        Top = 48
        Width = 181
        Height = 34
        Margins.Left = 8
        Margins.Top = 8
        Margins.Right = 8
        Margins.Bottom = 8
        Caption = 'From Address:'
      end
      object lblRecipientAddress: TLabel
        Left = 40
        Top = 178
        Width = 229
        Height = 34
        Margins.Left = 8
        Margins.Top = 8
        Margins.Right = 8
        Margins.Bottom = 8
        Caption = 'Recipient Address:'
      end
      object lblFromName: TLabel
        Left = 113
        Top = 98
        Width = 156
        Height = 34
        Margins.Left = 8
        Margins.Top = 8
        Margins.Right = 8
        Margins.Bottom = 8
        Caption = 'From Name:'
      end
      object lblRecipientName: TLabel
        Left = 65
        Top = 228
        Width = 204
        Height = 34
        Margins.Left = 8
        Margins.Top = 8
        Margins.Right = 8
        Margins.Bottom = 8
        Caption = 'Recipient Name:'
      end
      object lblSubject: TLabel
        Left = 167
        Top = 286
        Width = 102
        Height = 34
        Margins.Left = 8
        Margins.Top = 8
        Margins.Right = 8
        Margins.Bottom = 8
        Caption = 'Subject:'
      end
      object edtFromAddress: TEdit
        Left = 285
        Top = 37
        Width = 776
        Height = 42
        Margins.Left = 8
        Margins.Top = 8
        Margins.Right = 8
        Margins.Bottom = 8
        TabOrder = 0
      end
      object edtFromName: TEdit
        Left = 285
        Top = 95
        Width = 776
        Height = 42
        Margins.Left = 8
        Margins.Top = 8
        Margins.Right = 8
        Margins.Bottom = 8
        TabOrder = 1
      end
      object edtRecipientAddress: TEdit
        Left = 285
        Top = 167
        Width = 776
        Height = 42
        Margins.Left = 8
        Margins.Top = 8
        Margins.Right = 8
        Margins.Bottom = 8
        TabOrder = 2
      end
      object edtRecipientName: TEdit
        Left = 285
        Top = 225
        Width = 776
        Height = 42
        Margins.Left = 8
        Margins.Top = 8
        Margins.Right = 8
        Margins.Bottom = 8
        TabOrder = 3
      end
      object mmoBody: TMemo
        Left = 65
        Top = 360
        Width = 1016
        Height = 314
        Margins.Left = 8
        Margins.Top = 8
        Margins.Right = 8
        Margins.Bottom = 8
        Anchors = [akLeft, akTop, akRight, akBottom]
        Lines.Strings = (
          'Body Text')
        TabOrder = 4
      end
      object edtSubject: TEdit
        Left = 285
        Top = 283
        Width = 776
        Height = 42
        Margins.Left = 8
        Margins.Top = 8
        Margins.Right = 8
        Margins.Bottom = 8
        TabOrder = 5
        Text = 'Test Subject'
      end
    end
    object tsLogging: TTabSheet
      Margins.Left = 8
      Margins.Top = 8
      Margins.Right = 8
      Margins.Bottom = 8
      Caption = 'Logging'
      ImageIndex = 1
      object mmoLogging: TMemo
        Left = 0
        Top = 0
        Width = 1121
        Height = 682
        Margins.Left = 8
        Margins.Top = 8
        Margins.Right = 8
        Margins.Bottom = 8
        Align = alClient
        Lines.Strings = (
          'Memo1')
        ScrollBars = ssBoth
        TabOrder = 0
      end
    end
  end
  object btnSendHTMLMsg: TButton
    Left = 1200
    Top = 296
    Width = 314
    Height = 62
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Anchors = [akTop, akRight]
    Caption = 'Send MSG HTML Test'
    TabOrder = 8
    OnClick = btnSendHTMLMsgClick
  end
end
