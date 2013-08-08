unit KKUtils;

interface

uses
  Classes;

const
  KKEY_PLUS = '=';
  KKEY_MINUS = '-';
  KKEY_OK = #13;
  KKEY_CANCEL = #27;

//  CD_ROOT = 'E:\';
  CD_ROOT = 'C:\Documents and Settings\Deluan Cotts\My Documents\Projetos\KlingKlang\tmp\';

procedure CheckFreeSpace(Dir : String; var TotalSize, FreeSpace: Double);
function FormatDiskSpace(MB: Double): String;
function HashCode(S: String): Integer;
function CopyFile(Src, Dest: String): Boolean;


implementation

uses SysUtils, IniFiles;

procedure CheckFreeSpace(Dir : String; var TotalSize, FreeSpace: Double);
var AvailToCall, TS, FS : Int64;
begin
    FreeSpace := 0;
    GetDiskFreeSpaceEx(Pchar(Dir),
                       AvailToCall, TS, @FS);
    TotalSize := TS / 1048576; //convertendo de Byte para MByte
    FreeSpace := FS / 1048576; //convertendo de Byte para MByte
end;

function FormatDiskSpace(MB: Double): String;
begin
     if (MB > 1024) then
        Result := FormatFloat('0', MB /1024) + ' GB'
     else
        Result := FormatFloat('0', MB) + ' MB';
end;

function HashCode(S: String): Integer;
var I: Integer;
begin
     Result := 0;
     for I := 1 to Length(S) do begin
         Result := (Result shl 5) - Result + Integer(S[I]);
     end;
end;

function CopyFile(Src, Dest: String): Boolean;
var S, D: TFileStream;
begin
     try
        S := TFileStream.Create(Src, fmOpenRead);
        try
           D := TFileStream.Create(Dest, fmCreate);
           try
              D.CopyFrom(S, 0);
              Result := True;
           finally
              D.Free;
           end;
        finally
           S.Free;
        end;
     except
        Result := False;
     end;
end;

end.
