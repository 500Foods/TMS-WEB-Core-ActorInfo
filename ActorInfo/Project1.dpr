program Project1;

uses
  Vcl.Forms,
  Unit1 in 'Unit1.pas' {ServerContainer: TDataModule},
  Unit2 in 'Unit2.pas' {MainForm},
  ActorInfoService in 'ActorInfoService.pas',
  ActorInfoServiceImplementation in 'ActorInfoServiceImplementation.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TServerContainer, ServerContainer);
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
