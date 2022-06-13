unit Unit1;

interface

uses
  System.SysUtils, System.Classes, Sparkle.HttpServer.Module,
  Sparkle.HttpServer.Context, Sparkle.Comp.Server,
  Sparkle.Comp.HttpSysDispatcher, Aurelius.Drivers.Interfaces,
  Aurelius.Comp.Connection, XData.Comp.ConnectionPool, XData.Server.Module,
  XData.Comp.Server, XData.Aurelius.ModelBuilder,
  Sparkle.Comp.CorsMiddleware;

type
  TServerContainer = class(TDataModule)
    SparkleHttpSysDispatcher: TSparkleHttpSysDispatcher;
    XDataServer: TXDataServer;
    XDataConnectionPool: TXDataConnectionPool;
    AureliusConnection: TAureliusConnection;
    XDataServerCORS: TSparkleCorsMiddleware;
    procedure DataModuleCreate(Sender: TObject);
  end;

var
  ServerContainer: TServerContainer;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

procedure TServerContainer.DataModuleCreate(Sender: TObject);
begin
  TXDataModelBuilder.LoadXMLDoc(XDataServer.Model);
  XDataServer.Model.Title := 'Actor API';
  XDataServer.Model.Version := '1.0';
  XDataServer.Model.Description :=
    '### Overview'#13#10 +
    'This is an API for accessing stuff.';
end;

end.
