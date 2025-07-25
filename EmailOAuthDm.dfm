object EmailOAuthDataModule: TEmailOAuthDataModule
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Height = 1064
  Width = 1762
  PixelsPerInch = 240
  object IdSSLIOHandlerSocketPOP: TTaurusTLSIOHandlerSocket
    Destination = ':110'
    Intercept = IdConnectionPOP
    MaxLineAction = maException
    Port = 110
    DefaultPort = 0
    SSLOptions.MinTLSVersion = TLSv1_3
    SSLOptions.Mode = sslmClient
    SSLOptions.VerifyMode = []
    SSLOptions.VerifyDepth = 0
    Left = 1504
    Top = 304
  end
  object IdPOP3: TIdPOP3
    Intercept = IdConnectionPOP
    IOHandler = IdSSLIOHandlerSocketPOP
    AuthType = patSASL
    AutoLogin = False
    SASLMechanisms = <>
    Left = 1496
    Top = 104
  end
  object IdSMTP1: TIdSMTP
    Intercept = IdConnectionInterceptSMTP
    IOHandler = IdSSLIOHandlerSocketSMTP
    AuthType = satSASL
    SASLMechanisms = <>
    Left = 216
    Top = 529
  end
  object IdConnectionInterceptSMTP: TIdConnectionIntercept
    OnReceive = IdConnectionReceive
    OnSend = IdConnectionSend
    Left = 224
    Top = 104
  end
  object IdSSLIOHandlerSocketSMTP: TTaurusTLSIOHandlerSocket
    Destination = ':25'
    Intercept = IdConnectionInterceptSMTP
    MaxLineAction = maException
    Port = 25
    DefaultPort = 0
    SSLOptions.MinTLSVersion = TLSv1_3
    SSLOptions.Mode = sslmClient
    SSLOptions.VerifyMode = []
    SSLOptions.VerifyDepth = 0
    Left = 216
    Top = 328
  end
  object IdHTTPServer1: TIdHTTPServer
    Bindings = <>
    DefaultPort = 2132
    OnCommandGet = IdHTTPServer1CommandGet
    Left = 832
    Top = 752
  end
  object IdConnectionInterceptIMAP: TIdConnectionIntercept
    OnReceive = IdConnectionReceive
    OnSend = IdConnectionSend
    Left = 1040
    Top = 520
  end
  object IdConnectionPOP: TIdConnectionIntercept
    OnReceive = IdConnectionReceive
    OnSend = IdConnectionSend
    Left = 1512
    Top = 504
  end
  object IdIMAP: TIdIMAP4
    Intercept = IdConnectionInterceptIMAP
    IOHandler = IdSSLIOHandlerSocketIMAP
    UseTLS = utUseRequireTLS
    SASLMechanisms = <>
    AuthType = iatSASL
    MilliSecsToWaitToClearBuffer = 10
    Left = 1040
    Top = 96
  end
  object IdSSLIOHandlerSocketIMAP: TTaurusTLSIOHandlerSocket
    Destination = ':143'
    Intercept = IdConnectionInterceptIMAP
    MaxLineAction = maException
    Port = 143
    DefaultPort = 0
    SSLOptions.MinTLSVersion = TLSv1_3
    SSLOptions.Mode = sslmClient
    SSLOptions.VerifyMode = []
    SSLOptions.VerifyDepth = 0
    Left = 1048
    Top = 312
  end
  object RESTResponseGraph: TRESTResponse
    Left = 664
    Top = 528
  end
  object RESTRequestGraph: TRESTRequest
    Client = RESTClientGraph
    Params = <>
    Response = RESTResponseGraph
    SynchronizedEvents = False
    Left = 664
    Top = 312
  end
  object RESTClientGraph: TRESTClient
    BaseURL = 'https://graph.microsoft.com/v1.0'
    Params = <>
    SynchronizedEvents = False
    Left = 656
    Top = 104
  end
end
