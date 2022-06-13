unit ActorInfoService;

interface

uses
  XData.Service.Common,
  XData.Security.Attributes,
  Aurelius.Mapping.Attributes,
  System.Classes;


type
  [ServiceContract]
  IActorInfoService = interface(IInvokable)
    ['{D0697F1E-EE4C-47D1-A29E-0B19B5D396FD}']

    ///  <summary>
    ///    Birthdays
    ///  </summary>
    ///  <remarks>
    ///    Returns a list of Actors with a birthday that falls on the supplied month and day.
    ///  </remarks>
    ///  <param name="Secret">
    ///    If I told you, it wouldn't be a secret.
    ///  </param>
    ///  <param name="BirthMonth">
    ///    Birth Month (1-12).
    ///  </param>
    ///  <param name="BirthDay">
    ///    Birth Day (1-31).
    ///  </param>

    [HttpGet] function Birthdays(Secret: String; BirthMonth: Integer; BirthDay: Integer):TStream;

    ///  <summary>
    ///    Birthdays
    ///  </summary>
    ///  <remarks>
    ///    Returns a list of Actors with a birthday that falls on the supplied month and day.
    ///    This extended edition also includes photos, roles and other information.
    ///  </remarks>
    ///  <param name="Secret">
    ///    If I told you, it wouldn't be a secret.
    ///  </param>
    ///  <param name="BirthMonth">
    ///    Birth Month (1-12).
    ///  </param>
    ///  <param name="BirthDay">
    ///    Birth Day (1-31).
    ///  </param>

    [HttpGet] function BirthdaysExtended(Secret: String; BirthMonth: Integer; BirthDay: Integer):TStream;
  end;

implementation

initialization
  RegisterServiceType(TypeInfo(IActorInfoService));

end.
