unit Unit2;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Unit1, system.json, system.generics.collections, system.netencoding;

type
  TMainForm = class(TForm)
    mmInfo: TMemo;
    btStart: TButton;
    btStop: TButton;
    sparqlACTORS: TMemo;
    btSaveBirthdays: TButton;
    btLoadBirthdays: TButton;
    edSecret: TEdit;
    edTMDbAPI: TEdit;
    edSecretBase64: TEdit;
    lbSecret: TLabel;
    lbTMDbAPI: TLabel;
    Label1: TLabel;
    procedure btStartClick(ASender: TObject);
    procedure btStopClick(ASender: TObject);
    procedure FormCreate(ASender: TObject);
    procedure btSaveBirthdaysClick(Sender: TObject);
    procedure btLoadBirthdaysClick(Sender: TObject);
    procedure edSecretChange(Sender: TObject);
  public
    Birthdays: Array[1..366] of UTF8String;
    BirthdaysExtended: Array[1..366] of UTF8String;
    Actors: Array of UTF8String;
  strict private
    procedure UpdateGUI;
  end;

var
  MainForm: TMainForm;
  BirthdayCache: String;
  BirthdayExtendedCache: String;

implementation

{$R *.dfm}

resourcestring
  SServerStopped = 'Server stopped';
  SServerStartedAt = 'Server started at ';

{ TMainForm }

procedure TMainForm.btLoadBirthdaysClick(Sender: TObject);
var
  data: TStringList;
  jsondata: TJSONArray;
  i:Integer;
begin
  // Load birthdays from a file as JSON
  data := TSTringList.Create;
  try
    data.LoadFromFile(BirthdayCache);
  except on E: Exception do
    begin
      mmInfo.Lines.Add('WARNING: Birthday Cache file not loaded: '+BirthdayCache);
    end;
  end;
  if data.text <> '' then
  begin
    jsondata := TJSONObject.ParseJSONValue(data.text) as TJSONArray;
    for i := 0 to 365 do
    begin
      Birthdays[i+1] := (jsondata.items[i] as TJSONArray).ToString;
    end;
  end;

  // Load extended birthdays from a file as JSON
  data := TSTringList.Create;
  try
    data.LoadFromFile(BirthdayExtendedCache);
  except on E: Exception do
    begin
      mmInfo.Lines.Add('WARNING: Birthday Extended Cache file not loaded: '+BirthdayExtendedCache);
    end;
  end;
  if data.text <> '' then
  begin
    jsondata := TJSONObject.ParseJSONValue(data.text) as TJSONArray;
    for i := 0 to 365 do
    begin
      BirthdaysExtended[i+1] := (jsondata.items[i] as TJSONArray).ToString;
    end;
  end;

end;

procedure TMainForm.btSaveBirthdaysClick(Sender: TObject);
var
 data :TStringList;
 i: integer;
begin
  // Save birthdays to a file as JSON
  data := TStringList.Create;
  data.Add('[');
  for i := 1 to 366 do
  begin
    if i < 366
    then data.Add(Birthdays[i]+',')
    else data.Add(Birthdays[i]);
  end;
  data.Add(']');
  data.SavetoFile(BirthdayCache);
  data.Free;

  // Save extended birthdays to a file as JSON
  data := TStringList.Create;
  data.Add('[');
  for i := 1 to 366 do
  begin
    if i < 366
    then data.Add(BirthdaysExtended[i]+',')
    else data.Add(BirthdaysExtended[i]);
  end;
  data.Add(']');
  data.SavetoFile(BirthdayExtendedCache);

end;

procedure TMainForm.btStartClick(ASender: TObject);
begin
  ServerContainer.SparkleHttpSysDispatcher.Start;
  UpdateGUI;
end;

procedure TMainForm.btStopClick(ASender: TObject);
begin
  ServerContainer.SparkleHttpSysDispatcher.Stop;
  UpdateGUI;
end;

procedure TMainForm.edSecretChange(Sender: TObject);
begin
  edSecretBase64.Text := TNetEncoding.Base64.encode(edSecret.Text);
end;

procedure TMainForm.FormCreate(ASender: TObject);
var
  i: Integer;
begin

  // Initialize Birthdays to empty JSON arrays
  for i := 1 to 366 do
  begin
    Birthdays[i] := '[]';
    BirthdaysExtended[i] := '[]';
  end;

  // Load Birthday cache from disk, if available
  BirthdayCache := 'birthday.cache';
  BirthdayExtendedCache := 'birthday-extended.cache';
  btLoadBirthDaysClick(ASender);

  // Show encoded Base64 version of secret
  edSecretChange(ASender);

  UpdateGUI;
end;


procedure TMainForm.UpdateGUI;
const
  cHttp = 'http://+';
  cHttpLocalhost = 'http://localhost';
begin
  btStart.Enabled := not ServerContainer.SparkleHttpSysDispatcher.Active;
  btStop.Enabled := not btStart.Enabled;
  if ServerContainer.SparkleHttpSysDispatcher.Active then
    mmInfo.Lines.Add(SServerStartedAt + StringReplace(
      ServerContainer.XDataServer.BaseUrl,
      cHttp, cHttpLocalhost, [rfIgnoreCase]))
  else
    mmInfo.Lines.Add(SServerStopped);
end;

end.
