object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'TMS XData Server'
  ClientHeight = 748
  ClientWidth = 909
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    909
    748)
  PixelsPerInch = 96
  TextHeight = 13
  object lbSecret: TLabel
    Left = 8
    Top = 41
    Width = 65
    Height = 21
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Secret'
  end
  object lbTMDbAPI: TLabel
    Left = 8
    Top = 68
    Width = 65
    Height = 21
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'TMDb API'
  end
  object Label1: TLabel
    Left = 346
    Top = 66
    Width = 239
    Height = 21
    Alignment = taCenter
    AutoSize = False
    Caption = 'Secret encoded as Base64'
  end
  object mmInfo: TMemo
    Left = 8
    Top = 96
    Width = 893
    Height = 644
    Anchors = [akLeft, akTop, akRight, akBottom]
    ReadOnly = True
    TabOrder = 0
  end
  object btStart: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Start'
    TabOrder = 1
    OnClick = btStartClick
  end
  object btStop: TButton
    Left = 90
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Stop'
    TabOrder = 2
    OnClick = btStopClick
  end
  object sparqlACTORS: TMemo
    Left = 688
    Top = 8
    Width = 213
    Height = 82
    Lines.Strings = (
      'SELECT DISTINCT ?itemLabel ?DOB ?TMDbID WHERE '
      '{'
      '  ?item p:P106  ?statement_0;'
      '        p:P569  ?statement_1; '
      '        p:P4985 ?statement_2.'
      '  '
      '  ?statement_0 (ps:P106) wd:Q33999.'
      '  ?statement_1 psv:P569 ?statementValue_1.'
      '  ?statement_2 ps:P4985 ?TMDbID.'
      ''
      '  ?statementValue_1 wikibase:timeValue ?DOB.'
      '  FILTER (month(?DOB) = :MONTH )'
      '  FILTER (day(?DOB) = :DAY )'
      ''
      
        '  SERVICE wikibase:label { bd:serviceParam wikibase:language "en' +
        '". }'
      '}'
      '')
    TabOrder = 3
    Visible = False
    WordWrap = False
  end
  object btSaveBirthdays: TButton
    Left = 171
    Top = 8
    Width = 122
    Height = 25
    Caption = 'Save Birthday Cache'
    TabOrder = 4
    OnClick = btSaveBirthdaysClick
  end
  object btLoadBirthdays: TButton
    Left = 299
    Top = 8
    Width = 122
    Height = 25
    Caption = 'Load Birthday Cache'
    TabOrder = 5
    OnClick = btLoadBirthdaysClick
  end
  object edSecret: TEdit
    Left = 90
    Top = 39
    Width = 250
    Height = 21
    TabOrder = 6
    Text = 'LeelooDallasMultiPass'
    OnChange = edSecretChange
  end
  object edTMDbAPI: TEdit
    Left = 90
    Top = 66
    Width = 250
    Height = 21
    TabOrder = 7
    Text = '<< insert your own TMDb API key here >>'
  end
  object edSecretBase64: TEdit
    Left = 346
    Top = 39
    Width = 250
    Height = 21
    TabOrder = 8
  end
end
