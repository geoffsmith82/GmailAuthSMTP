object EmailOAuthDataModule: TEmailOAuthDataModule
  OldCreateOrder = True
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Height = 3000
  Width = 4000
  object IdSSLIOHandlerSocketPOP: TIdSSLIOHandlerSocketOpenSSL
    Destination = ':110'
    MaxLineAction = maException
    Port = 110
    DefaultPort = 0
    SSLOptions.Method = sslvTLSv1_2
    SSLOptions.SSLVersions = [sslvTLSv1_2]
    SSLOptions.Mode = sslmClient
    SSLOptions.VerifyMode = []
    SSLOptions.VerifyDepth = 0
    Left = 632
    Top = 248
  end
  object IdPOP3: TIdPOP3
    Intercept = IdConnectionPOP
    IOHandler = IdSSLIOHandlerSocketPOP
    AuthType = patSASL
    AutoLogin = False
    SASLMechanisms = <>
    Left = 624
    Top = 440
  end
  object IdSMTP1: TIdSMTP
    Intercept = IdConnectionInterceptSMTP
    IOHandler = IdSSLIOHandlerSocketSMTP
    AuthType = satSASL
    SASLMechanisms = <>
    UseTLS = utUseRequireTLS
    Left = 184
    Top = 448
  end
  object IdConnectionInterceptSMTP: TIdConnectionIntercept
    OnReceive = IdConnectionReceive
    OnSend = IdConnectionSend
    Left = 176
    Top = 80
  end
  object IdSSLIOHandlerSocketSMTP: TIdSSLIOHandlerSocketOpenSSL
    Destination = ':25'
    Intercept = IdConnectionInterceptSMTP
    MaxLineAction = maException
    Port = 25
    DefaultPort = 0
    SSLOptions.Method = sslvTLSv1_2
    SSLOptions.SSLVersions = [sslvTLSv1_2]
    SSLOptions.Mode = sslmClient
    SSLOptions.VerifyMode = []
    SSLOptions.VerifyDepth = 0
    Left = 176
    Top = 264
  end
  object IdHTTPServer1: TIdHTTPServer
    Bindings = <>
    DefaultPort = 2132
    OnCommandGet = IdHTTPServer1CommandGet
    Left = 1464
    Top = 80
  end
  object IdConnectionInterceptIMAP: TIdConnectionIntercept
    OnReceive = IdConnectionReceive
    OnSend = IdConnectionSend
    Left = 1096
    Top = 72
  end
  object IdConnectionPOP: TIdConnectionIntercept
    OnReceive = IdConnectionReceive
    OnSend = IdConnectionSend
    Left = 640
    Top = 80
  end
  object IdIMAP: TIdIMAP4
    Intercept = IdConnectionInterceptIMAP
    IOHandler = IdSSLIOHandlerSocketIMAP
    UseTLS = utUseRequireTLS
    SASLMechanisms = <>
    AuthType = iatSASL
    MilliSecsToWaitToClearBuffer = 10
    Left = 1088
    Top = 440
  end
  object IdSSLIOHandlerSocketIMAP: TIdSSLIOHandlerSocketOpenSSL
    Destination = ':143'
    Intercept = IdConnectionInterceptIMAP
    MaxLineAction = maException
    Port = 143
    DefaultPort = 0
    SSLOptions.Method = sslvTLSv1_2
    SSLOptions.SSLVersions = [sslvTLSv1_2]
    SSLOptions.Mode = sslmClient
    SSLOptions.VerifyMode = []
    SSLOptions.VerifyDepth = 0
    Left = 1088
    Top = 236
  end
end
