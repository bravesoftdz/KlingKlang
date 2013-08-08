unit FormMain;

interface

uses
  SysUtils, Types, Classes, Variants, QTypes, QGraphics, QControls, QForms,
  QDialogs, QStdCtrls, QExtCtrls, DBXpress, DB, DBClient, SimpleDS, SqlExpr,
  IniFiles, FMTBcd;

type
  TForm1 = class(TForm)
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    edtTitle: TEdit;
    edtArtist: TEdit;
    edtYear: TEdit;
    mmoDescription: TMemo;
    Label6: TLabel;
    mmoTracks: TMemo;
    Button2: TButton;
    Button3: TButton;
    dlgOpenFolder: TOpenDialog;
    Image1: TImage;
    dlgOpenCover: TOpenDialog;
    SQLConnection: TSQLConnection;
    sdsTitle: TSimpleDataSet;
    sdsTrack: TSimpleDataSet;
    Button1: TButton;
    sqlNewTitle: TSQLQuery;
    Label1: TLabel;
    edtCaption: TEdit;
    procedure Button3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Image1DblClick(Sender: TObject);
  private
    { Private declarations }
    CoverPath: String;
    TitlePath: String;
    Tracks: TStringList;
    procedure CopyFromPlaylist(FileName: String);
    procedure SaveToDB;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.xfm}

procedure TForm1.Button3Click(Sender: TObject);
begin
     Application.Terminate;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
     SQLConnection.Close;
     SQLConnection.Params.Values['Database'] := 'Data\KlingKlang.fdb';
     SQLConnection.Open;
     Tracks := TStringList.Create;
end;

procedure TForm1.CopyFromPlaylist(FileName: String);
var Ini: TIniFile;
    I, Count: Integer;
    SR: TSearchRec;
    Track: String;
begin
     Ini := TIniFile.Create(FileName);
     try
        TitlePath := ExtractFilePath(FileName);
        edtTitle.Text := Ini.ReadString('playlist', 'title', '');
        edtArtist.Text := Ini.ReadString('playlist', 'artist', '');
        edtCaption.Text := edtArtist.Text + ' - ' + edtTitle.Text;
        edtCaption.Text := Ini.ReadString('playlist', 'caption', edtCaption.Text);
        edtYear.Text := Ini.ReadString('playlist', 'year', '');
        mmoDescription.Lines.Text := Ini.ReadString('playlist', 'description', '');
        Tracks.Clear;
        mmoTracks.Clear;
        Count := Ini.ReadInteger('playlist', 'NumberOfEntries', 0);
        for I := 1 to Count do begin
            Track := Ini.ReadString('playlist', 'File' + IntToStr(I), '');
            Tracks.Add(Track);
            SetLength(Track, Length(Track)-4);
            Track := StringReplace(Track, '_', '/', [rfReplaceAll]);
            mmoTracks.Lines.Add(Track);
        end;
        if FindFirst(TitlePath + '\*.*', faAnyFile, SR) = 0 then
           repeat
                 try
                    CoverPath := TitlePath + '\' + SR.Name;
                    Image1.Picture.LoadFromFile(CoverPath);
                    Break;
                 except
                 end;
           until FindNext(SR) <> 0
     finally
        Ini.Free;
     end;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
     if dlgOpenFolder.Execute then begin
        CopyFromPlaylist(dlgOpenFolder.FileName);
     end;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
     SaveToDB;
end;

procedure TForm1.SaveToDB;
var Key: Integer;
    I: Integer;
    Track: String;
begin
     sdsTitle.Open;
     if sdsTitle.Locate('TITLENAME; ARTISTNAME', VarArrayOf([edtTitle.Text, edtArtist.Text]), [loCaseInsensitive]) then begin
        ShowMessage('Este título já consta no cadastro!');
        Exit;
     end;
     sqlNewTitle.Close;
     sqlNewTitle.Open;
     Key := sqlNewTitle.Fields[0].AsInteger + 1;
     with sdsTitle do begin
          Insert;
          FieldByName('TitleNum').AsInteger := Key;
          FieldByName('TitleName').AsString := edtTitle.Text;
          FieldByName('ArtistName').AsString := edtArtist.Text;
          FieldByName('Description').AsString := mmoDescription.Lines.Text;
          FieldByName('ReleaseYear').AsInteger := StrToInt(edtYear.Text);
          FieldByName('Caption').AsString := edtCaption.Text;
          FieldByName('CoverFile').AsString := CoverPath;
          Post;
          ApplyUpdates(-1);
     end;
     with sdsTrack do begin
          Close;
          Open;
          for I := 0 to mmoTracks.Lines.Count-1 do begin
              Insert;
              Track := mmoTracks.Lines[I];
              FieldByName('TitleNum').AsInteger := Key;
              FieldByName('TrackNum').AsInteger := I+1;
              FieldByName('Sequence').AsInteger := I+1;
              FieldByName('Path').AsString := TitlePath + Tracks[I];
              FieldByName('TrackName').AsString := mmoTracks.Lines[I];
              FieldByName('Available').AsString := 'T';
              Post;
          end;
          ApplyUpdates(-1);
     end;
end;

procedure TForm1.Image1DblClick(Sender: TObject);
begin
     if dlgOpenCover.Execute then begin
        try
           Image1.Picture.LoadFromFile(dlgOpenFolder.FileName);
           CoverPath := dlgOpenFolder.FileName;
        except
           CoverPath := '';   
        end;
     end;
end;

end.
