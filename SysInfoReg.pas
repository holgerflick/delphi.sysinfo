unit SysInfoReg;

interface

procedure Register;

implementation

uses
  System.Classes, SysInfo;

procedure Register;
begin
  RegisterComponents('System', [TSysInfo]);
end;

end.
