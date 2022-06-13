object ServerContainer: TServerContainer
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Height = 210
  Width = 431
  object SparkleHttpSysDispatcher: TSparkleHttpSysDispatcher
    Active = True
    Left = 72
    Top = 16
  end
  object XDataServer: TXDataServer
    BaseUrl = 'http://+:2001/tms/xdata'
    Dispatcher = SparkleHttpSysDispatcher
    Pool = XDataConnectionPool
    EntitySetPermissions = <>
    SwaggerOptions.Enabled = True
    SwaggerUIOptions.Enabled = True
    SwaggerUIOptions.ShowFilter = True
    SwaggerUIOptions.DocExpansion = Full
    Left = 216
    Top = 16
    object XDataServerCORS: TSparkleCorsMiddleware
      Origin = '*'
    end
  end
  object XDataConnectionPool: TXDataConnectionPool
    Connection = AureliusConnection
    Left = 216
    Top = 72
  end
  object AureliusConnection: TAureliusConnection
    Left = 216
    Top = 128
  end
end
