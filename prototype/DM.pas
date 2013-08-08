unit DM;

interface

uses
  SysUtils, Classes, QTypes, DBXpress, DB, SqlExpr, QGraphics, FMTBcd;

type
  TdtmKlingKlang = class(TDataModule)
    SQLConnection: TSQLConnection;
    sqlTitles: TSQLQuery;
    sqlTracks: TSQLQuery;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
    { Private declarations }
    FTitles: TStringList;
    procedure InitTitleList;
    procedure ClearTitlesList;
  public
    { Public declarations }
    property Titles: TStringList read FTitles;
  end;

  TTitle = class
  private
    FTitleNum: Integer;
    FCaption: String;
    FTitleName: String;
    FArtistName: String;
    FYear: Integer;
    FDescription: String;
    FTracks: TStringList;
    FCover: TBitmap;
    FCoverFile: String;
    function GetCover: TBitmap;
    function GetTracks: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    property TitleNum: Integer read FTitleNum;
    property Caption: String read FCaption;
    property TitleName: String read FTitleName;
    property ArtistName: String read FArtistName;
    property Year: Integer read FYear;
    property Description: String read FDescription;
    property Tracks: TStringList read GetTracks;
    property Cover: TBitmap read GetCover;
  end;

var
  dtmKlingKlang: TdtmKlingKlang;

implementation

{$R *.xfm}


{ TdtmKlingKlang }

procedure TdtmKlingKlang.DataModuleCreate(Sender: TObject);
begin
     SQLConnection.CloneConnection;
     SQLConnection.Params.Values['Database'] := 'Data\KlingKlang.fdb';
     FTitles := TStringList.Create;
     FTitles.Sorted := False;
     FTitles.CaseSensitive := False;
     InitTitleList;
end;

procedure TdtmKlingKlang.InitTitleList;
var Title: TTitle;
begin
     sqlTitles.Open;
     sqlTitles.First;
     ClearTitlesList;
     while not sqlTitles.Eof do begin
           Title := TTitle.Create;
           with Title do begin
                FTitleNum := sqlTitles.FieldByName('TitleNum').AsInteger;
                FCaption := sqlTitles.FieldByName('Caption').AsString;
                FTitleName := sqlTitles.FieldByName('TitleName').AsString;
                FArtistName := sqlTitles.FieldByName('ArtistName').AsString;
                FYear := sqlTitles.FieldByName('ReleaseYear').AsInteger;
                FDescription := sqlTitles.FieldByName('Description').AsString;
                FCoverFile := sqlTitles.FieldByName('CoverFile').AsString;
           end;
           FTitles.AddObject(Title.FCaption, Title);
           sqlTitles.Next;
     end;
     FTitles.Sort;
     sqlTitles.Close;
end;


procedure TdtmKlingKlang.DataModuleDestroy(Sender: TObject);
begin
     ClearTitlesList;
     FTitles.Free;
end;

procedure TdtmKlingKlang.ClearTitlesList;
var I: Integer;
begin
     for I := 0 to FTitles.Count-1 do
         FTitles.Objects[I].Free;
     FTitles.Clear;
end;

{ TTitle }

constructor TTitle.Create;
begin
     inherited Create;
     FTracks := Nil;
     FCover := Nil;
end;

destructor TTitle.Destroy;
begin
     if Assigned(FTracks) then
        FreeAndNil(FTracks);
     if Assigned(FCover) then
        FreeAndNil(FCover);
     inherited;
end;

function TTitle.GetCover: TBitmap;
begin
     if not Assigned(FCover) then begin
        FCover := TBitmap.Create;
        try
           FCover.LoadFromFile(FCoverFile);
        except
           FreeAndNil(FCover);
        end;
     end;
     Result := FCover;
end;

function TTitle.GetTracks: TStringList;
begin
     if not Assigned(FTracks) then begin
        FTracks := TStringList.Create;
        with dtmKlingKlang.sqlTracks do begin
             Close;
             ParamByName('TitleNum').AsInteger := FTitleNum;
             Open;
             First;
             while not Eof do begin
                   FTracks.Add(FormatFloat('00 ', FieldByName('Sequence').AsInteger) +
                               FieldByName('TrackName').AsString + '=' +
                               FieldByName('Path').AsString);
                   Next;
             end;
             Close;
             FTracks.Sort;
        end;
     end;
     Result := FTracks;
end;

end.
