unit ActorInfoServiceImplementation;

interface

uses
  System.Classes,
  System.SysUtils,
  System.DateUtils,
  System.StrUtils,
  System.JSON,
  System.Math,
  System.NetEncoding,
  System.Generics.Collections,

  REST.JSON,

  XData.Server.Module,
  XData.Service.Common,
  XData.Sys.Exceptions,

  IdHTTP, IdSSLOpenSSL, idURI,

  ActorInfoService;

type
  [ServiceImplementation]
  TActorInfoService = class(TInterfacedObject, IActorInfoService)

      function Birthdays(Secret: String; BirthMonth: Integer; BirthDay: Integer):TStream;
      function BirthdaysExtended(Secret: String; BirthMonth: Integer; BirthDay: Integer):TStream;

  end;

implementation

uses Unit2;

{ TActorInfoService }

function TActorInfoService.Birthdays(Secret: String; BirthMonth, BirthDay: Integer):TStream;
var
  sparql: TIdHTTP;                            // The request being sent to WikiDATA
  SSLHandler: TIdSSLIOHandlerSocketOpenSSL ;  // SSL stuff
  qry: String;                                // The query (needs to be encoded)
  req: UTF8String;                            // The response coming back
  data: TJSONArray;                           // The response paired down to the relevant JSON array
  cacheindex: Integer;                        // Julian day of birthday selected
  cacheentry: UTF8String;                     // New value of cache
  i: integer;
begin

  // First, did they send the correct secret?
  if (Secret <> MainForm.edSecret.Text) then raise EXDataHttpUnauthorized.Create('Access Not Authorized');

  // Second, did they request a valid day?
  try
    cacheindex := DayOfTheYear(EncodeDate(2000, BirthMonth, BirthDay));
  except on E: Exception do
    begin
      raise EXDataHttpException.Create('Invalid Birthday');
    end;
  end;

  // Alright, seems like we've got a valid request.
  Result := TMemoryStream.Create;
  TXDataOperationContext.Current.Response.Headers.SetValue('content-type', 'application/json');

  // Is a cached result available?
  if Mainform.Birthdays[cacheindex] <> '[]' then
  begin
    cacheentry := Mainform.Birthdays[cacheindex];
    Result.WriteBuffer(Pointer(cacheentry)^, length(cacheentry));
  end
  else
  begin
    // Get SPARQL query from TMemo (easier to edit there)
    // First we replace the tokens with whatever date we've selected
    // and then encode it so that it can be passed as part of the URL
    // NOTE: We want the result to come back as JSON, not XML
    qry := MainForm.sparqlACTORS.Lines.Text;
    qry := StringReplace(StringReplace(qry, ':MONTH', IntToStr(BirthMonth), [rfReplaceAll]), ':DAY', IntToStr(BirthDay), [rfReplaceAll]);
    qry := TidURI.URLEncode('https://query.wikidata.org/sparql?query='+qry+'&format=json');

    // Bunch of stuff to support SSL.
    // Need to install latest OpenSSL Win64 DLL's in debug folder
    // Can get the latest version from: https://indy.fulgan.com/SSL/
    SSLHandler := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
    SSLHandler.SSLOptions.Method := sslvTLSv1_2;

    // Create the HTTP Request - defaults to httpGET
    // Note: httpPOST isn't going to work with this endpoint
    sparql := TidHTTP.Create(nil);
    sparql.IOHandler := SSLHandler ; //set HTTP IOHandler for SSL Connection

    try
      req := sparql.Get(qry);
      data := ((TJSONObject.ParseJSONValue(req) as TJSONObject).getValue('results') as TJSONObject).getValue('bindings') as TJSONArray;
      cacheentry := '[';
      for i := 0 to data.Count - 1 do
      begin
        cacheentry := cacheentry+'{';
        cacheentry := cacheentry+'"Name":'+REST.JSON.TJSON.JSONEncode(((data.Items[i] as TJSONObject).getValue('itemLabel') as TJSONObject).getValue('value') as TJSONString)+',';
        cacheentry := cacheentry+'"DOB":"' +(((data.Items[i] as TJSONObject).getValue('DOB')       as TJSONObject).getValue('value') as TJSONString).value+'",';
        cacheentry := cacheentry+'"TMDb":"'+(((data.Items[i] as TJSONObject).getValue('TMDbID')    as TJSONObject).getValue('value') as TJSONString).value+'"';
        cacheentry := cacheentry+'}';
        if i < data.count - 1
        then cacheentry := cacheentry+',';
      end;
      cacheentry := cacheentry+']';
      MainForm.Birthdays[cacheindex] := cacheentry;
    except on E: Exception do
      begin
        raise EXDataHttpException.Create('Nope');
      end;
    end;
    Result.WriteBuffer(Pointer(cacheentry)^, length(cacheentry));
  end;
end;

function TActorInfoService.BirthdaysExtended(Secret: String; BirthMonth, BirthDay: Integer): TStream;
var
  TMDb: TIdHTTP;                              // The request being sent to TMDb
  SSLHandler: TIdSSLIOHandlerSocketOpenSSL ;  // SSL stuff
  qry: String;                                // What we're asking for
  data: TJSONObject;                          // Response converted to JSON
  role: TJSONObject;                          // A particular role found in the data
  actors: TJSONArray;
  req: UTF8String;
  RolePopularityTV: TSTringList;
  RolePopularityMovie: TSTringList;
  response: UTF8String;
  cacheindex: Integer;
  cacheentry: UTF8String;

  i: integer;  // used for iterating list of roles (movies, tv shows)
  j: integer;  // used for iterating list of birthdays from original birthday list
begin
//  MainForm.mmInfo.Lines.Add(FormatDateTime('hh:nn:ss',Now)+' Processing Request');

  // First, did they send the correct secret?
  if (Secret <> MainForm.edSecret.Text) then raise EXDataHttpUnauthorized.Create('Access Not Authorized');

  // Second, did they request a valid day?
  try
    cacheindex := DayOfTheYear(EncodeDate(2000, BirthMonth, BirthDay));
  except on E: Exception do
    begin
      raise EXDataHttpException.Create('Invalid Birthday');
    end;
  end;

  // Alright, seems like we've got a valid request.
  Result := TMemoryStream.Create;
  TXDataOperationContext.Current.Response.Headers.SetValue('content-type', 'application/json');

  // If the Birthday cache doesn't exist, then get the birthdays.
  // We need a list of TMDb IDs that comes from that last to generate a new extended list.
  if MainForm.Birthdays[cacheindex] = '[]' then
  begin
    Birthdays(Secret, BirthMonth, BirthDay);
  end;

//  MainForm.mmInfo.Lines.Add(FormatDateTime('hh:nn:ss',Now)+' Continuing Request');

  // If the BirthdaysExtended cache exist, great!  We don't have to do anything.
  if MainForm.BirthdaysExtended[cacheindex] <> '[]' then
  begin
    cacheentry := Mainform.BirthdaysExtended[cacheindex];
    Result.WriteBuffer(Pointer(cacheentry)^, length(cacheentry));
  end

  // Otherwise, we do.
  else
  begin

    // Bunch of stuff to support SSL.
    // Need to install latest OpenSSL Win64 DLL's in debug folder
    // Can get the latest version from: https://indy.fulgan.com/SSL/
    SSLHandler := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
    SSLHandler.SSLOptions.Method := sslvTLSv1_2;

    // Create the HTTP Request - defaults to httpGET
    // Note: httpPOST isn't going to work with this endpoint
    TMDb := TidHTTP.Create(nil);
    TMDb.IOHandler := SSLHandler ; //set HTTP IOHandler for SSL Connection

    actors := TJSONObject.ParseJSONValue(MainForm.BirthDays[cacheindex]) as TJSONArray;
    response := '[';

    for j := 0 to (actors.Count - 1) do
    begin

      // What are we asking for?
      qry := 'https://api.themoviedb.org/3/';

//    qry := qry+'person/738';   // Sean Connery
//    qry := qry+'person/30084'; // Anna Torv
//    qry := qry+'person/524';   // Natalie Portman

      qry := qry+'person/'+((actors.Items[j] as TJSONObject).getValue('TMDb') as TJSONString).Value;
      qry := qry+'?api_key='+MainForm.edTMDbAPI.Text;
      qry := qry+'&language=en-US';
      qry := qry+'&append_to_response=image,combined_credits';
      qry := TidURI.URLEncode(qry);

      req := '';
      try
        req := TMDb.Get(qry);
      except on E:Exception do
        begin
        end;
      end;

      data := TJSONObject.ParseJSONValue(req) as TJSONObject;

      if (req <> '') and ((data.getValue('adult') as TJSONBool).asBoolean = False) then
      begin

//      MainForm.mmInfo.Lines.Add(FormatDateTime('hh:nn:ss',Now)+' Formatting Request: '+IntToStr(j+1)+' of '+IntToStr(Actors.Count));

        // Name - Assume it is always present?
        response := response+'{"Name":'+REST.JSON.TJSON.JSONEncode(data.getValue('name') as TJSONString)+',';

        // Birthday - We got the birthday from Wikipedia, but not always set (or even the same?!) in TMDb.
        // So here, we'll just use the Wikipedia version.
        response := response+'"DOB":"'+((actors.Items[j] as TJSONObject).getValue('DOB') as TJSONString).Value+'",';
//        if (data.getValue('birthday') is TJSONNULL)
//        then response := response+'"DOB":null,'
//        else response := response+'"DOB":"'+(data.getValue('birthday') as TJSONString).Value+'",';

        // Deathday
        if (data.getValue('deathday') is TJSONNULL)
        then response := response+'"DOD":null,'
        else response := response+'"DOD":"'+(data.getValue('deathday') as TJSONString).Value+'",';

        // Popularity - a TMDb scoring metric
        if (data.getValue('popularity') is TJSONNULL)
        then response := response+'"Pop":0,'
        else response := response+'"Pop":'+FloatToStr((data.getValue('popularity') as TJSONNumber).AsDouble)+',';

        // Biography
        if (data.getValue('biography') is TJSONNULL)
        then response := response+'"BIO":null,'
        else response := response+'"BIO":'+REST.JSON.TJSON.JSONEncode(data.getValue('biography') as TJSONString)+',';

        // Birthplace
        if (data.getValue('place_of_birth') is TJSONNULL)
        then response := response+'"BP":null,'
        else response := response+'"BP":'+REST.JSON.TJSON.JSONEncode(data.getValue('place_of_birth') as TJSONString)+',';

        // Path to get photo
        if (data.getValue('profile_path') is TJSONNULL)
        then response := response+'"IMGLNK":null,'
        else
        begin
          response := response+'"IMGLNK":"'+'https://image.tmdb.org/t/p/w185'+StringReplace((data.getValue('profile_path') as TJSONString).Value,'"','',[rfReplaceAll])+'",';

          // if we wanted to include the image in the JSON, we could do it like this.
//        req := TMDB.Get('https://image.tmdb.org/t/p/w185'+StringReplace((data.getValue('profile_path') as TJSONString).Value,'"','',[rfReplaceAll]));
//        response := response+'"IMG":"'+TNetEncoding.URL.encodeBytesToString(bytesOf(StringReplace(TNetEncoding.Base64.Encode(req),chr(10),'',[rfReplaceAll])))+'",';
        end;

        // Number of roles
        response := response+'"Roles":'+IntToStr(((data.getValue('combined_credits') as TJSONOBject).getValue('cast') as TJSONArray).Count)+',';

        // Find top 3 most popular roles
        // This is a bit of a mess as the popularity figure is completely different for TV vs. Movies.
        // So we separate them out and sort them to get the top three of each, and list the movies first.
        // On the client we'll decide how many of these to show, if any.

        RolePopularityTV := TStringList.Create;
        RolePopularityMovie := TStringList.Create;

        for i := 0 to ((data.getValue('combined_credits') as TJSONObject).getValue('cast') as TJSONArray).Count - 1 do
        begin
          role := (((data.getValue('combined_credits') as TJSONOBject).getValue('cast') as TJSONArray).Items[i] as TJSONObject);

          if (role.getValue('popularity') <> nil) then
          begin
            if (role.getValue('media_type') <> nil) then
            begin
              if ((role.getValue('media_type') as TJSONString).Value = 'tv')    then RolePopularityTV.Add(RightStr('00000000'+IntToStr(Trunc(100000000-(role.getValue('popularity') as TJSONNumber).AsDouble*1000.0)),8)+'/'+IntToStr(i));
              if ((role.getValue('media_type') as TJSONString).Value = 'movie') then RolePopularityMovie.Add(RightStr('00000000'+IntToStr(Trunc(100000000-(role.getValue('popularity') as TJSONNumber).AsDouble*1000.0)),8)+'/'+IntToStr(i));
            end;
          end;
        end;

        RolePopularityTV.Sort;
        RolePopularityMovie.Sort;

        i := 0;
        if (RolePopularityMovie.count > 0) then
        begin
          for I := 0 to min(2, RolePopularityMovie.count-1) do
          begin
            role := (((data.getValue('combined_credits') as TJSONOBject).getValue('cast') as TJSONArray).Items[StrToInt(Copy(RolePopularityMovie[i],Pos('/',RolePopularityMovie[i])+1,8))] as TJSONObject);

            if (role.getValue('popularity') = nil)
            then response:= response+'"Pop_'+IntToStr(i)+'":null,'
            else response:= response+'"Pop_'+IntToStr(i)+'":'+FloatToSTr((role.getValue('popularity') as TJSONNumber).AsDouble)+',';

            if (role.getValue('media_type') = nil)
            then response:= response+'"Type_'+IntToStr(i)+'":null,'
            else response:= response+'"Type_'+IntToStr(i)+'":"'+(role.getValue('media_type') as TJSONString).Value+'",';

            if (role.getValue('title') = nil)
            then response:= response+'"Title_'+IntToStr(i)+'":null,'
            else response:= response+'"Title_'+IntToStr(i)+'":'+REST.JSON.TJSON.JSONEncode(role.getValue('title') as TJSONString)+',';

            if (role.getValue('release_date') = nil)
            then response:= response+'"Released_'+IntToStr(i)+'":null,'
            else response:= response+'"Released_'+IntToStr(i)+'":"'+(role.getValue('release_date') as TJSONString).Value+'",';

            if (role.getValue('poster_path') = nil)
            then response:= response+'"Poster_'+IntToStr(i)+'":null,'
            else if (role.getValue('poster_path') is TJSONNULL)
               then response:= response+'"Poster_'+IntToStr(i)+'":null,'
               else response:= response+'"Poster_'+IntToStr(i)+'":"'+'https://image.tmdb.org/t/p/w185'+(role.getValue('poster_path') as TJSONString).Value+'",';
          end;
        end;

        i := 0;
        if (RolePopularityTV.Count > 0) then
        begin
          for i := 0 to min(2, RolePopularityTV.count-1) do
          begin
            role := (((data.getValue('combined_credits') as TJSONOBject).getValue('cast') as TJSONArray).Items[StrToInt(Copy(RolePopularityTV[i],Pos('/',RolePopularityTV[i])+1,8))] as TJSONObject);

            if (role.getValue('popularity') = nil)
            then response:= response+'"Pop_'+IntToStr(i+10)+'":null,'
            else response:= response+'"Pop_'+IntToStr(i+10)+'":'+FloatToStr((role.getValue('popularity') as TJSONNumber).AsDouble)+',';

            if (role.getValue('media_type') = nil)
            then response:= response+'"Type_'+IntToStr(i+10)+'":null,'
            else response:= response+'"Type_'+IntToStr(i+10)+'":"'+(role.getValue('media_type') as TJSONString).Value+'",';

            if (role.getValue('name') = nil)
            then response:= response+'"Title'+IntToStr(i+10)+'":null,'
            else response:= response+'"Title_'+IntToStr(i+10)+'":'+REST.JSON.TJSON.JSONEncode(role.getValue('name') as TJSONString)+',';

            if (role.getValue('first_air_date') = nil)
            then response:= response+'"Released_'+IntToStr(i+10)+'":null,'
            else response:= response+'"Released_'+IntToStr(i+10)+'":"'+(role.getValue('first_air_date') as TJSONString).Value+'",';

            if (role.getValue('poster_path') = nil)
            then response:= response+'"Poster_'+IntToStr(i+10)+'":null,'
            else if (role.getValue('poster_path') is TJSONNULL)
                 then response:= response+'"Poster_'+IntToStr(i+10)+'":null,'
                 else response:= response+'"Poster_'+IntToStr(i+10)+'":"'+'https://image.tmdb.org/t/p/w185'+(role.getValue('poster_path') as TJSONString).Value+'",';
          end;
        end;

        response := response + '"Work":[';
        for i := 0 to ((data.getValue('combined_credits') as TJSONObject).getValue('cast') as TJSONArray).Count - 1 do
        begin
          role := (((data.getValue('combined_credits') as TJSONOBject).getValue('cast') as TJSONArray).Items[i] as TJSONObject);

          if (role.getValue('popularity') <> nil) then
          begin
            if (role.getValue('media_type') <> nil) then
            begin
              if ((role.getValue('media_type') as TJSONString).Value = 'tv')    then
              begin
                if (role.getValue('popularity') = nil)
                then response:= response+'{"Pop":null,'
                else response:= response+'{"Pop":'+FloatToStr((role.getValue('popularity') as TJSONNumber).AsDouble)+',';

                if (role.getValue('media_type') = nil)
                then response:= response+'"Type":null,'
                else response:= response+'"Type":"'+(role.getValue('media_type') as TJSONString).Value+'",';

                if (role.getValue('name') = nil)
                then response:= response+'"Title":null,'
                else response:= response+'"Title":'+REST.JSON.TJSON.JSONEncode(role.getValue('name') as TJSONString)+',';

                if (role.getValue('first_air_date') = nil)
                then response:= response+'"Released":null,'
                else response:= response+'"Released":"'+(role.getValue('first_air_date') as TJSONString).Value+'",';

                if (role.getValue('poster_path') = nil)
                then response:= response+'"Poster":null,'
                else if (role.getValue('poster_path') is TJSONNULL)
                     then response:= response+'"Poster":null,'
                     else response:= response+'"Poster":"'+'https://image.tmdb.org/t/p/w185'+(role.getValue('poster_path') as TJSONString).Value+'",';
              end

              else if ((role.getValue('media_type') as TJSONString).Value = 'movie') then
              begin
                if (role.getValue('popularity') = nil)
                then response:= response+'{"Pop":null,'
                else response:= response+'{"Pop":'+FloatToSTr((role.getValue('popularity') as TJSONNumber).AsDouble)+',';

                if (role.getValue('media_type') = nil)
                then response:= response+'"Type":null,'
                else response:= response+'"Type":"'+(role.getValue('media_type') as TJSONString).Value+'",';

                if (role.getValue('title') = nil)
                then response:= response+'"Title":null,'
                else response:= response+'"Title":'+REST.JSON.TJSON.JSONEncode(role.getValue('title') as TJSONString)+',';

                if (role.getValue('release_date') = nil)
                then response:= response+'"Released":null,'
                else response:= response+'"Released":"'+(role.getValue('release_date') as TJSONString).Value+'",';

                if (role.getValue('poster_path') = nil)
                then response:= response+'"Poster":null,'
                else if (role.getValue('poster_path') is TJSONNULL)
                     then response:= response+'"Poster":null,'
                     else response:= response+'"Poster":"'+'https://image.tmdb.org/t/p/w185'+(role.getValue('poster_path') as TJSONString).Value+'",';
              end;
            end;
            if  i <  ((data.getValue('combined_credits') as TJSONObject).getValue('cast') as TJSONArray).Count - 1
            then response := response+'"end":"end"},'
            else response := response+'"end":"end"}'
          end;
        end;

        response := response +'],';

        if j < Actors.Count -1
        then response := response+'"end":"end"},'
        else response := response+'"end":"end"}';

        RolePopularityTV.Free;
        RolePopularityMovie.Free;
      end;
    end;

    response := response+']';
    response := StringReplace(response, chr(9), '', [rfReplaceAll]);
    MainForm.BirthdaysExtended[cacheindex] := response;
    Result.WriteBuffer(Pointer(response)^, length(response));

//    MainForm.mmInfo.Lines.Add(FormatDateTime('hh:nn:ss',Now)+' Request Complete.');

  end;

end;

initialization
  RegisterServiceType(TActorInfoService);

end.
