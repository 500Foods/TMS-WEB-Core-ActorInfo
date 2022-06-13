unit Unit2;

interface

uses
  System.SysUtils, System.Classes, JS, Web, WEBLib.Graphics, WEBLib.Controls,
  WEBLib.Forms, WEBLib.Dialogs, Vcl.Controls, WEBLib.WebCtrls,
  Vcl.StdCtrls, WEBLib.StdCtrls, WEBLib.REST;

type
  TForm2 = class(TWebForm)
    divTabulator: TWebHTMLDiv;
    divFlatPickr: TWebHTMLDiv;
    divTabulator2: TWebHTMLDiv;
    divPhoto: TWebHTMLDiv;
    divBiography: TWebHTMLDiv;
    procedure WebFormCreate(Sender: TObject);
    [async] procedure GetBirthdays(aMonth: Integer; aDay: Integer);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

procedure TForm2.WebFormCreate(Sender: TObject);
begin

  asm
    var GetBirthdays = this.GetBirthdays;
    flatpickr("#divFlatPickr", {
      inline: true,
      appendTo: divFlatPickr,
      onChange: function(selectedDates, dateStr, instance) {
        GetBirthdays(parseInt(dateStr.substr(5,2)), parseInt(dateStr.substr(8,2)));
      }
    });

    var checkimage = function(value, data, type, params, component){
      //value - original value of the cell
      //data - the data for the row
      //type - the type of mutation occurring  (data|edit)
      //params - the mutatorParams object from the column definition
      //component - when the "type" argument is "edit", this contains the cell component for the edited cell, otherwise it is the column component for the column

      if (value == null) {
        if (params.imgtype == "person") return 'img/person-placeholder.jpg';
        if (params.imgtype == "movie")  return 'img/movie-placeholder.jpg';
        if (params.imgtype == "tv")     return 'img/tv-placeholder.jpg';
        if (params.imgtype == "tvmovie") {
          if (data.Type == 'tv') return 'img/tv-placeholder.jpg'
          else return 'img/movie-placeholder.jpg';
        }
      }
      else return value;
    }

    var tabulator = new Tabulator("#divTabulator", {
      layout: "fitColumns",
      selectable: 1,
      initialSort:[ {column:"Pop", dir:"desc"} ],
 	    columns:[
        { title: "", field: "IMGLNK", width:60, headerSort:false, resizable: false, hozAlign:"center",formatter: "image", formatterParams:{height:50, width:33},
          mutator: checkimage, mutatorParams: {imgtype: "person"}},
        { title: "Name", field: "Name", bottomCalc: "count",vertAlign:"middle" },
        { title: "Birthdate", field: "DOB", width:125, formatter: "datetime", formatterParams: {inputFormat: "iso", outputFormat:"yyyy-MMM-dd", timezone:"UTC"},vertAlign:"middle" },
        { title: "Roles", field: "Roles", width:95, hozAlign: "right",vertAlign:"middle" },
        { title: "Pop", field: "Pop",width:95,sorter:"number", hozAlign: "right",vertAlign:"middle", formatter:"money", formatterParams:{
          decimal: ".",
          thousand:",",
          symbol:"",
          symbolAfter:"",
          precision: 3
        }},

        {field: "Title_0",  visible:false },
        {field: "Title_1",  visible:false },
        {field: "Title_2",  visible:false },
        {field: "Title_10", visible:false },
        {field: "Title_11", visible:false },
        {field: "Title_12", visible:false },

        { title: "", field: "Poster_0", width:60, headerSort:false, resizable: false, hozAlign:"center",formatter: "image", formatterParams:{height:50, width:33},
          mutator: checkimage, mutatorParams: {imgtype: "movie"}, tooltip:function(e, cell, onRendered){ return cell.getRow().getCell('Title_0').getValue()}},
        { title: "", field: "Poster_1", width:60, headerSort:false, resizable: false, hozAlign:"center",formatter: "image", formatterParams:{height:50, width:33},
          mutator: checkimage, mutatorParams: {imgtype: "movie"}, tooltip:function(e, cell, onRendered){ return cell.getRow().getCell('Title_1').getValue()}},
        { title: "", field: "Poster_2", width:60, headerSort:false, resizable: false, hozAlign:"center",formatter: "image", formatterParams:{height:50, width:33},
          mutator: checkimage, mutatorParams: {imgtype: "movie"}, tooltip:function(e, cell, onRendered){ return cell.getRow().getCell('Title_2').getValue()}},
        { title: "", field: "Poster_10", width:60, headerSort:false, resizable: false, hozAlign:"center",formatter: "image", formatterParams:{height:50, width:33},
          mutator: checkimage, mutatorParams: {imgtype: "tv"}, tooltip:function(e, cell, onRendered){ return cell.getRow().getCell('Title_10').getValue()}},
        { title: "", field: "Poster_11", width:60, headerSort:false, resizable: false, hozAlign:"center",formatter: "image", formatterParams:{height:50, width:33},
          mutator: checkimage, mutatorParams: {imgtype: "tv"}, tooltip:function(e, cell, onRendered){ return cell.getRow().getCell('Title_11').getValue()}},
        { title: "", field: "Poster_12", width:60, headerSort:false, resizable: false, hozAlign:"center",formatter: "image", formatterParams:{height:50, width:33},
          mutator: checkimage, mutatorParams: {imgtype: "tv"}, tooltip:function(e, cell, onRendered){ return cell.getRow().getCell('Title_12').getValue()}},

        {field: "Work",  visible:false },
        {field: "BIO",  visible:false }

      ]
    });
    tabulator.on("rowClick", function(e, row){
      //e - the click event object
      //row - row component

      // Get data for subtable from Work field in main table
      var data = row.getCell('Work').getValue();
      var subtable = Tabulator.findTable("#divTabulator2")[0];
      subtable.replaceData(data);

      // Load photo from main table into DIV
      var photo = document.getElementById('divPhoto');
      photo.innerHTML = '<img class="rounded shadow" height=300 width=200 src='+row.getCell('IMGLNK').getValue()+'>';

      // Load biography from main table into DIV
      var bio = document.getElementById('divBiography');
      bio.innerHTML = '<div style="width:430px; height:300px;overflow-y:scroll;" class="rounded border border-light p-2 shadow">'+row.getCell('BIO').getValue()+'</div>';
    });

    var tabulator2 = new Tabulator("#divTabulator2", {
      layout: "fitColumns",
      selectable: 1,
      initialSort:[ {column:"Pop", dir:"desc"} ],
 	    columns:[
        { title: "", field: "Poster", width:60, headerSort:false, resizable: false, hozAlign:"center",formatter: "image", formatterParams:{height:50, width:33},
          mutator: checkimage, mutatorParams: {imgtype: "tvmovie"}},
        { title: "Title", field: "Title", bottomCalc: "count",vertAlign:"middle" },
        { title: "Type", field: "Type", width:125, vertAlign:"middle" },
        { title: "Released", field: "Released", width:125, formatter: "datetime", formatterParams: {inputFormat: "iso", outputFormat:"yyyy-MMM-dd", timezone:"UTC"},vertAlign:"middle" },
        { title: "Pop", field: "Pop", width: 95, sorter: "number", hozAlign: "right",vertAlign:"middle", formatter:"money", formatterParams:{
          decimal: ".",
          thousand:",",
          symbol:"",
          symbolAfter:"",
          precision: 3
        }}
      ]
    });

  end;

end;

procedure TForm2.GetBirthdays(aMonth: Integer; aDay: Integer);
var
  birthdays: TWebHTTPRequest;  // The request being sent to our XData service endpoint
  req: TJSXMLHttpRequest;      // The response coming back
  data :WideString;            // The response coming back, as text
  good: Boolean;               // Indicates whether we've got data
begin

  birthdays := TWebHTTPRequest.Create(nil);
  birthdays.URL := 'http://localhost:2001/tms/xdata/ActorInfoService/BirthdaysExtended';
  birthdays.URL := birthdays.URL+'?Secret='+window.atob('TGVlbG9vRGFsbGFzTXVsdGlQYXNz');
  birthdays.URL := birthdays.URL+'&BirthMonth='+IntToStr(amonth);
  birthdays.URL := birthdays.URL+'&BirthDay='+IntToStr(aDay);
  good := false;
  try
    req := await(TJSXMLHttpRequest, birthdays.Perform());
    data := req.responseText;
    good := True;
  except
    showmessage('nope');
  end;

  // If we're successful, load data into the table
  if good then
  begin
    asm
      var table = Tabulator.findTable("#divTabulator")[0];
      table.replaceData(JSON.parse(data));
    end;
  end;
end;

end.