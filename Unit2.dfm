object Form2: TForm2
  Left = 0
  Top = 0
  Margins.Left = 1
  Margins.Top = 1
  Margins.Right = 1
  Margins.Bottom = 1
  Caption = 'Test OAUTH2 Gmail Send Message'
  ClientHeight = 556
  ClientWidth = 607
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    607
    556)
  TextHeight = 13
  object btnAuthenticate: TButton
    Left = 474
    Top = 8
    Width = 128
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Authenticate'
    TabOrder = 0
    OnClick = btnAuthenticateClick
  end
  object btnSendMsg: TButton
    Left = 474
    Top = 87
    Width = 126
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Send MSG'
    TabOrder = 1
    OnClick = btnSendMsgClick
  end
  object rgEmailProviders: TRadioGroup
    Left = 8
    Top = 8
    Width = 337
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
    Left = 474
    Top = 210
    Width = 126
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Check MSG'#39's'
    TabOrder = 3
    OnClick = btnCheckMsgClick
  end
  object btnClearAuthToken: TButton
    Left = 474
    Top = 39
    Width = 126
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Clear Auth Token'
    TabOrder = 4
    OnClick = btnClearAuthTokenClick
  end
  object btnCheckIMAP: TButton
    Left = 474
    Top = 250
    Width = 126
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Check IMAP'
    TabOrder = 5
    OnClick = btnCheckIMAPClick
  end
  object btnSendViaREST: TButton
    Left = 474
    Top = 161
    Width = 126
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Send MSG via REST'
    TabOrder = 6
    OnClick = btnSendViaRESTClick
  end
  object PageControl1: TPageControl
    Left = 8
    Top = 72
    Width = 456
    Height = 479
    ActivePage = tsEmail
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 7
    object tsEmail: TTabSheet
      Caption = 'Email'
      DesignSize = (
        448
        451)
      object lblFrom: TLabel
        Left = 35
        Top = 19
        Width = 70
        Height = 13
        Caption = 'From Address:'
      end
      object lblRecipientAddress: TLabel
        Left = 16
        Top = 71
        Width = 90
        Height = 13
        Caption = 'Recipient Address:'
      end
      object lblFromName: TLabel
        Left = 45
        Top = 39
        Width = 58
        Height = 13
        Caption = 'From Name:'
      end
      object lblRecipientName: TLabel
        Left = 26
        Top = 91
        Width = 78
        Height = 13
        Caption = 'Recipient Name:'
      end
      object lblSubject: TLabel
        Left = 67
        Top = 114
        Width = 40
        Height = 13
        Caption = 'Subject:'
      end
      object edtFromAddress: TEdit
        Left = 114
        Top = 15
        Width = 310
        Height = 21
        TabOrder = 0
      end
      object edtFromName: TEdit
        Left = 114
        Top = 38
        Width = 310
        Height = 21
        TabOrder = 1
      end
      object edtRecipientAddress: TEdit
        Left = 114
        Top = 67
        Width = 310
        Height = 21
        TabOrder = 2
      end
      object edtRecipientName: TEdit
        Left = 114
        Top = 90
        Width = 310
        Height = 21
        TabOrder = 3
      end
      object mmoBody: TMemo
        Left = 26
        Top = 144
        Width = 401
        Height = 79
        Anchors = [akLeft, akTop, akRight, akBottom]
        Lines.Strings = (
          'Body Text')
        TabOrder = 4
      end
      object edtSubject: TEdit
        Left = 114
        Top = 113
        Width = 310
        Height = 21
        TabOrder = 5
        Text = 'Test Subject'
      end
      object mmoLogging: TMemo
        Left = 26
        Top = 232
        Width = 401
        Height = 216
        Anchors = [akLeft, akTop, akBottom]
        Lines.Strings = (
          'Memo1')
        ScrollBars = ssBoth
        TabOrder = 6
      end
    end
  end
  object btnSendHTMLMsg: TButton
    Left = 474
    Top = 118
    Width = 126
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Send MSG HTML Test'
    TabOrder = 8
    OnClick = btnSendHTMLMsgClick
  end
  object chkPKCE: TCheckBox
    Left = 397
    Top = 12
    Width = 63
    Height = 17
    Caption = 'PKCE'
    Checked = True
    State = cbChecked
    TabOrder = 9
  end
end
