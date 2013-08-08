unit DataModule;

interface

uses
  SysUtils, Classes, QTypes, DB, DBClient, QGraphics, IniFiles, LbCipher,
  LbClass;

type
  TTitle = class;

  TTrack = class;

  TKKData = class(TDataModule)
    cdsTitles: TClientDataSet;
    cdsTitlesTitleId: TIntegerField;
    cdsTitlesTitleName: TStringField;
    cdsTitlesArtistName: TStringField;
    cdsTitlesTitleCaption: TStringField;
    cdsTitlesReleaseYear: TIntegerField;
    cdsTitlesDescription: TMemoField;
    cdsTitlesCoverId: TIntegerField;
    cdsTitlesVisible: TBooleanField;
    cdsTracks: TClientDataSet;
    cdsTracksTrackId: TIntegerField;
    cdsTracksTrackName: TStringField;
    cdsTracksTrackPath: TIntegerField;
    cdsTracksDuration: TIntegerField;
    cdsTracksVolume: TIntegerField;
    cdsQueue: TClientDataSet;
    cdsQueueQueuedTime: TDateTimeField;
    cdsQueueTrackId: TIntegerField;
    cdsQueueTitleId: TIntegerField;
    cdsState: TClientDataSet;
    cdsStateCreditCount: TIntegerField;
    cdsStateForcedCredits: TIntegerField;
    cdsStateVolume: TIntegerField;
    cdsRewards: TClientDataSet;
    cdsGranted: TClientDataSet;
    cdsTitlesTracksPath: TIntegerField;
    cdsTitlesSize: TFloatField;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
    FTitles: TStringList;
    FHiddenTitles: TStringList;
    FVolume: Integer;
    FCredits: Integer;
    FLastUpdate: TDateTime;
    procedure ClearTitlesList;
    procedure LoadTitleList;
    procedure LoadDataSet(cds: TClientDataSet; FileName: String);
    function SaveDataSet(cds: TClientDataSet; FileName: String): Boolean;
    procedure LoadTracks(TitlePath: Integer);
    procedure SetVolume(const Value: Integer);
    procedure SaveState;
    function FindTrack(TitleList: TStringList; TitleId, TrackId: Integer): TTrack;
  public

    // Controle de créditos
    procedure AddCredit(Forced: Boolean);
    procedure UseCredit;

    // Manipulação da Fila
    procedure LoadQueue(Queue: TStringList);
    procedure AddToQueue(Track: TTrack);
    procedure TrackCompleted;

    // Administração de títulos
    procedure RemoveTitle(Title: TTitle);
    procedure HideTitle(Title: TTitle);
    procedure UnHideTitle(Title: TTitle);

    property LastUpdate: TDateTime read FLastUpdate;
    property Titles: TStringList read FTitles;
    property HiddenTitles: TStringList read FHiddenTitles;
    property Volume: Integer read FVolume write SetVolume;
    property Credits: Integer read FCredits;
  end;

  TTrack = class
  private
    FTitle: TTitle;
    FTitleId: Integer;
    FTrackId: Integer;
    FTrackPath: Integer;
    FTrackName: String;
    FVolume: Integer;
    FSourcePath: String;
    procedure SetTrackPath(Value: String);
    function GetTrackPath: String;
    function GetCaption: String;
    function GetArtistName: String;
  public
    function Import: Boolean;
    property TrackName: String read FTrackName;
    property TrackPath: String read GetTrackPath write SetTrackPath;
    property Volume: Integer read FVolume write FVolume;
    property ArtistName: String read GetArtistName;
    property Caption: String read GetCaption;
  end;

  TTitle = class
  private
    FTitleId: Integer;
    FCaption: String;
    FTitleName: String;
    FArtistName: String;
    FYear: Integer;
    FDescription: String;
    FTracks: TStringList;
    FTracksPath: Integer;
    FCover: TBitmap;
    FCoverPath: Integer;
    FCoverSourceFile: String;
    FSize: Double;
    FVisible: Boolean;
    procedure InitTracks;
    function GetCover: TBitmap;
    function GetTrack(Index: Integer): TTrack;
    function GetTrackCount: Integer;
    function GetTracks: TStringList;
    procedure RemoveFiles;
    procedure ClearMemory;
    procedure Assign(ATitle: TTitle);
  public
    constructor Create;
    destructor Destroy; override;
    property TitleId: Integer read FTitleId;
    property Caption: String read FCaption;
    property TitleName: String read FTitleName;
    property ArtistName: String read FArtistName;
    property Year: Integer read FYear;
    property Size: Double read FSize;
    property Visible: Boolean read FVisible;
    property Description: String read FDescription;
    property Tracks[Index: Integer]: TTrack read GetTrack;
    property TrackCount: Integer read GetTrackCount;
    property TrackList: TStringList read GetTracks;
    property Cover: TBitmap read GetCover;
    procedure AddTrack(ATrack: TTrack);
    procedure Import;
    function LoadFromPlaylist(PlsPath: String): Boolean;
  end;

  TKKMasterCD = class
  private
    FTitles: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    function Load: Boolean;
    property Titles: TStringList read FTitles;
  end;


var
  KKData: TKKData;

implementation

uses KKUtils, StrUtils;

{$R *.xfm}

const
  DATAPATH  = 'Data' + PathDelim;

  TITLEDAT  = '8E';
  QUEUEDAT  = 'BC';
  STATEDAT  = 'E1';
  REWARDDAT = 'C2';

function GetFilePath(F: Integer): String;
var S: String;
begin
     S := Format('%8.8x', [F]);
     Result := DATAPATH + S[8] + PathDelim + S[7] + PathDelim + S[6] + PathDelim;
     ForceDirectories(Result);
     Result := Result + Copy(S, 1, 5);
end;


{ TKKData }

procedure TKKData.DataModuleCreate(Sender: TObject);
begin
     FTitles := TStringList.Create;
     FTitles.Sorted := False;
     FTitles.CaseSensitive := False;
     FHiddenTitles := TStringList.Create;
     FHiddenTitles.Sorted := False;
     FHiddenTitles.CaseSensitive := False;
     LoadTitleList;

     LoadDataSet(cdsState, STATEDAT);
     if not cdsState.Eof then begin
        FVolume := cdsState.FieldByName('Volume').AsInteger;
        FCredits := cdsState.FieldByName('CreditCount').AsInteger;
     end
     else begin
        FVolume := 255;
        FCredits := 0;
     end;
     cdsState.Close;
end;

procedure TKKData.DataModuleDestroy(Sender: TObject);
begin
     ClearTitlesList;
end;

procedure TKKData.LoadTitleList;
var Title: TTitle;
begin
     LoadDataSet(cdsTitles, TITLEDAT);
     cdsTitles.First;
     ClearTitlesList;
     while not cdsTitles.Eof do begin
           Title := TTitle.Create;
           with Title do begin
                FTitleId := cdsTitles.FieldByName('TitleId').AsInteger;
                FTitleName := cdsTitles.FieldByName('TitleName').AsString;
                FArtistName := cdsTitles.FieldByName('ArtistName').AsString;
                FCaption := FArtistName + ' - ' + FTitleName;
                FYear := cdsTitles.FieldByName('ReleaseYear').AsInteger;
                FDescription := cdsTitles.FieldByName('Description').AsString;
                FSize := cdsTitles.FieldByName('Size').AsFloat;
                FCoverPath := cdsTitles.FieldByName('CoverPath').AsInteger;
                FTracksPath := cdsTitles.FieldByName('TracksPath').AsInteger;
                FVisible := cdsTitles.FieldByName('Visible').AsBoolean;
           end;
           if (Title.Visible) then
              FTitles.AddObject(Title.FCaption, Title)
           else
              FHiddenTitles.AddObject(Title.FCaption, Title);
           cdsTitles.Next;
     end;
     FTitles.Sort;
     FHiddenTitles.Sort;
     FLastUpdate := Now;
     cdsTitles.Close;
end;

procedure TKKData.ClearTitlesList;
var I: Integer;
begin
     for I := 0 to FTitles.Count-1 do
         FTitles.Objects[I].Free;
     FTitles.Clear;
     for I := 0 to FHiddenTitles.Count-1 do
         FHiddenTitles.Objects[I].Free;
     FHiddenTitles.Clear;
     FLastUpdate := Now;
end;

procedure TKKData.LoadDataSet(cds: TClientDataSet; FileName: String);
begin
     cds.Close;
     try
        cds.LoadFromFile(DATAPATH + FileName);
     except
        cds.CreateDataSet;
     end;
     cds.Open;
end;

procedure TKKData.LoadTracks(TitlePath: Integer);
begin
     cdsTracks.Close;
     try
        cdsTracks.LoadFromFile(GetFilePath(TitlePath));
     except
        cdsTracks.CreateDataSet;
     end;
     cdsTracks.Open;
end;


function TKKData.SaveDataSet(cds: TClientDataSet; FileName: String): Boolean;
begin
     try
        cds.MergeChangeLog;
        cds.SaveToFile(FileName + '.tmp');
        DeleteFile(FileName);
        Result := RenameFile(FileName + '.tmp', FileName);
        if (Result) then
           cds.Close;
     except
        Result := False;
     end;
end;

procedure TKKData.AddCredit(Forced: Boolean);
begin
  Inc(FCredits);
  SaveState;
end;

procedure TKKData.AddToQueue(Track: TTrack);
begin
     with cdsQueue do begin
          LoadDataSet(cdsQueue, QUEUEDAT);
          Append;
          FieldByName('QueuedTime').AsDatetime := Now;
          FieldByName('TitleId').AsInteger := Track.FTitle.FTitleId;
          FieldByName('TrackId').AsInteger := Track.FTrackId;
          Post;
          SaveDataSet(cdsQueue, DATAPATH + QUEUEDAT);
     end;
end;

procedure TKKData.TrackCompleted;
begin
     with cdsQueue do begin
          LoadDataSet(cdsQueue, QUEUEDAT);
          First;
          Delete;
          SaveDataSet(cdsQueue, DATAPATH + QUEUEDAT);
     end;
end;

function TKKData.FindTrack(TitleList: TStringList; TitleId, TrackId: Integer): TTrack;
var I, J: Integer;
    T: TTitle;
begin
     Result := Nil;
     for I := 0 to TitleList.Count-1 do begin
         T := TitleList.Objects[I] as TTitle;
         if (T.TitleId = TitleId) then begin
            for J := 0 to T.TrackCount-1 do begin
                if T.Tracks[J].FTrackId = TrackId then begin
                   Result := T.Tracks[J];
                   Exit;
                end;
            end;
         end;
     end;
end;

procedure TKKData.LoadQueue(Queue: TStringList);
var T: TTrack;
begin
     Queue.Clear;
     with cdsQueue do begin
          LoadDataSet(cdsQueue, QUEUEDAT);
          First;
          while not Eof do begin
                T := FindTrack(FTitles, FieldByName('TitleId').AsInteger,
                               FieldByName('TrackId').AsInteger);
                if not Assigned(T) then
                   T := FindTrack(FHiddenTitles, FieldByName('TitleId').AsInteger,
                               FieldByName('TrackId').AsInteger);
                if Assigned(T) then
                   Queue.AddObject(T.TrackName, T);
                Next;
          end;
          Close;
     end;
end;

procedure TKKData.SetVolume(const Value: Integer);
begin
  FVolume := Value;
  SaveState;
end;

procedure TKKData.SaveState;
begin
  with cdsState do begin
       LoadDataSet(cdsState, STATEDAT);
       First;
       if Eof then
          Insert
       else
          Edit;
       FieldByName('Volume').AsInteger := FVolume;
       FieldByName('CreditCount').AsInteger := FCredits;
       Post;
       SaveDataSet(cdsState, DATAPATH + STATEDAT);
  end;
end;

procedure TKKData.UseCredit;
begin
     if FCredits > 0 then begin
        Dec(FCredits);
        SaveState;
     end;
end;

procedure TKKData.RemoveTitle(Title: TTitle);
var Exists: Boolean;
    I: Integer;
begin
     with cdsTitles do begin
          LoadDataSet(cdsTitles, TITLEDAT);
          Exists := FindKey([Title.FTitleId]);
          if Exists then begin
             Delete;
             SaveDataSet(cdsTitles, DATAPATH + TITLEDAT);
          end
          else
             cdsTitles.Close;
     end;
     if Exists then begin
        for I := 0 to FTitles.Count-1 do
            if (Title.TitleId = (FTitles.Objects[I] as TTitle).TitleId) then begin
               FTitles.Delete(I);
               Break;
            end;
        for I := 0 to FHiddenTitles.Count-1 do
            if (Title.TitleId = (FHiddenTitles.Objects[I] as TTitle).TitleId) then begin
               FHiddenTitles.Delete(I);
               Break;
            end;
        Title.RemoveFiles;
        Title.Free;
        FLastUpdate := Now;
     end;
end;

procedure TKKData.HideTitle(Title: TTitle);
var I: Integer;
    T: TTitle;
begin
     for I := 0 to FTitles.Count-1 do begin
         T := FTitles.Objects[I] as TTitle;
         if (T.TitleId = Title.TitleId) then begin
            FTitles.Delete(I);
            T.FVisible := False;
            FHiddenTitles.AddObject(T.Caption, T);
            FHiddenTitles.Sort;
            FLastUpdate := Now;
            with cdsTitles do begin
                 LoadDataSet(cdsTitles, TITLEDAT);
                 if FindKey([T.FTitleId]) then begin
                    Edit;
                    FieldByName('Visible').AsBoolean := False;
                    Post;
                    SaveDataSet(cdsTitles, DATAPATH + TITLEDAT);
                 end
                 else
                    cdsTitles.Close;
            end;
            Break;
         end;
     end;
end;

procedure TKKData.UnHideTitle(Title: TTitle);
var I: Integer;
    T: TTitle;
begin
     for I := 0 to FHiddenTitles.Count-1 do begin
         T := FHiddenTitles.Objects[I] as TTitle;
         if (T.TitleId = Title.TitleId) then begin
            FHiddenTitles.Delete(I);
            T.FVisible := True;
            FTitles.AddObject(T.Caption, T);
            FTitles.Sort;
            FLastUpdate := Now;
            with cdsTitles do begin
                 LoadDataSet(cdsTitles, TITLEDAT);
                 if FindKey([T.FTitleId]) then begin
                    Edit;
                    FieldByName('Visible').AsBoolean := True;
                    Post;
                    SaveDataSet(cdsTitles, DATAPATH + TITLEDAT);
                 end
                 else
                    cdsTitles.Close;
            end;
            Break;
         end;
     end;
end;

{ TTitle }

constructor TTitle.Create;
begin
     inherited Create;
     FTracks := Nil;
     FCover := Nil;
     FVisible := True;
end;

destructor TTitle.Destroy;
begin
     ClearMemory;
     inherited;
end;

procedure TTitle.Assign(ATitle: TTitle);
begin
     ClearMemory;
     FTitleId := ATitle.FTitleId;
     FCaption := ATitle.FCaption;
     FTitleName := ATitle.FTitleName;
     FArtistName := ATitle.FArtistName;
     FYear := ATitle.FYear;
     FDescription := ATitle.FDescription;
     FTracksPath := ATitle.FTracksPath;
     FCoverPath := ATitle.FCoverPath;
     FSize :=  ATitle.FSize;
     FVisible := ATitle.FVisible;
end;

procedure TTitle.ClearMemory;
var I: Integer;
begin
     if Assigned(FTracks) then begin
        for I := 0 to FTracks.Count - 1 do
            FTracks.Objects[I].Free;
        FreeAndNil(FTracks);
     end;
     if Assigned(FCover) then
        FreeAndNil(FCover);
end;

function TTitle.GetCover: TBitmap;
begin
     if not Assigned(FCover) then begin
        FCover := TBitmap.Create;
        try
           FCover.LoadFromFile(GetFilePath(FCoverPath));
        except
           FreeAndNil(FCover);
        end;
     end;
     Result := FCover;
end;

function TTitle.GetTrack(Index: Integer): TTrack;
begin
     if not Assigned(FTracks) then
        InitTracks;
     Result := FTracks.Objects[Index] as TTrack;
end;

function TTitle.GetTrackCount: Integer;
begin
     if not Assigned(FTracks) then
        InitTracks;
     Result := FTracks.Count;
end;

procedure TTitle.InitTracks;
var T: TTrack;
begin
     if not Assigned(FTracks) then begin
        FTracks := TStringList.Create;
        FTracks.Sorted := False;
        KKData.LoadTracks(FTracksPath);
        with KKData.cdsTracks do begin
             First;
             while not Eof do begin
                   T := TTrack.Create;
                   T.FTitle := Self;
                   T.FTitleId := FTitleId;
                   T.FTrackId := FieldByName('TrackId').AsInteger;
                   T.FTrackPath := FieldByName('TrackPath').AsInteger;
                   T.FTrackName := FieldByName('TrackName').AsString;
                   T.FVolume := FieldByName('Volume').AsInteger;
                   FTracks.AddObject(T.Caption, T);
                   Next;
             end;
             Close;
             FTracks.Sort;
        end;
     end;
end;

procedure TTitle.AddTrack(ATrack: TTrack);
begin
     if not Assigned(FTracks) then
        InitTracks;
     ATrack.FTitle := Self;
     ATrack.FTitleId := FTitleId;
     FTracks.AddObject(ATrack.Caption, ATrack);
end;

procedure TTitle.Import;
var I: Integer;
    T: TTrack;
    NewTitle: TTitle;
begin
     // Importa as musicas primeiro. Se der qualquer erro, aborta...
     for I := 0 to FTracks.Count-1 do begin
         T := FTracks.Objects[I] as TTrack;
         if not T.Import then
            Exit;
     end;

     // Importa a capa
     FCoverPath := HashCode(FCaption + ' Cover');
     while FileExists(GetFilePath(FCoverPath)) do
           Inc(FCoverPath);
     if not CopyFile(FCoverSourceFile, GetFilePath(FCoverPath)) then
        Exit;
     FCoverSourceFile := '';

     // Cria o cds com as faixas
     with KKData, cdsTracks do begin
          CreateDataSet;
          for I := 0 to FTracks.Count-1 do begin
              T := FTracks.Objects[I] as TTrack;
              Insert;
              FieldByName('TrackId').AsInteger := T.FTrackId;
              FieldByName('TrackName').AsString := T.FTrackName;
              FieldByName('TrackPath').AsInteger := T.FTrackPath;
              FieldByName('Duration').AsInteger := 0;
              FieldByName('Volume').AsInteger := T.FVolume;
              Post;
          end;

          FTracksPath := HashCode(FCaption + ' Tracks');
          while FileExists(GetFilePath(FTracksPath)) do
                Inc(FTracksPath);
          if not SaveDataSet(cdsTracks, GetFilePath(FTracksPath)) then
             Exit;
     end;

     // Finalmente inclui o título na tabela de Títulos
     FTitleId := FTracksPath; // deve ser único...
     with KKData, cdsTitles do begin
          LoadDataSet(cdsTitles, TITLEDAT);
          Insert;
          FieldByName('TitleId').AsInteger := FTitleId;
          FieldByName('TitleName').AsString := FTitleName;
          FieldByName('ArtistName').AsString := FArtistName;
          FieldByName('TitleCaption').AsString := FCaption;
          FieldByName('ReleaseYear').AsInteger := FYear;
          FieldByName('Description').AsString := FDescription;
          FieldByName('Visible').AsBoolean := True;
          FieldByName('Size').AsFloat := FSize;
          FieldByName('TracksPath').AsInteger := FTracksPath;
          FieldByName('CoverPath').AsInteger := FCoverPath;
          Post;
          SaveDataSet(cdsTitles, DATAPATH + TITLEDAT);
     end;

     // Adiciona na lista de títulos 
     NewTitle := TTitle.Create;
     NewTitle.Assign(Self);
     KKData.FTitles.AddObject(FCaption, NewTitle);
     KKData.FTitles.Sort;
     KKData.FLastUpdate := Now;
end;

function TTitle.LoadFromPlaylist(PlsPath: String): Boolean;
var pls: TIniFile;
    I, Count: Integer;
    T: TTrack;
    S: String;
    SR: TSearchRec;
begin
     Result := False;
     pls := TIniFile.Create(PlsPath + PathDelim +'kk.pls');
     try
        if pls.ReadString('playlist', 'Size', '') = '' then
           Exit;

        // Limpa os dados anteriores (se existiam)
        ClearMemory;

        // Informações sobre o Título
        FTitleName := pls.ReadString('playlist', 'Title', '');
        FArtistName := pls.ReadString('playlist', 'Artist', '');
        S := FArtistName + ' - ' + FTitleName;
        FCaption := S;//pls.ReadString('playlist', 'Caption', S);
        FDescription := pls.ReadString('playlist', 'Description', '');
        FYear := pls.ReadInteger('playlist', 'Year', 0);
        S := pls.ReadString('playlist', 'Size', '0');
        S := StringReplace(S, '.', DecimalSeparator, [rfReplaceAll]);
        S := StringReplace(S, ',', DecimalSeparator, [rfReplaceAll]);
        FSize := StrToFloat(S);

        // Carrega as músicas
        FTracks := TStringList.Create;
        FTracks.Sorted := False;
        Count := pls.ReadInteger('playlist', 'NumberOfEntries', 0);
        for I := 1 to Count do begin
            S := pls.ReadString('playlist', 'File' + IntToStr(I), '');
            T := TTrack.Create;
            T.Volume := 255;
            T.TrackPath := PlsPath + PathDelim + S;
            SetLength(S, Length(S)-4);
            T.FTrackName := StringReplace(S, '_', '/', [rfReplaceAll]);
            T.FTrackId := I;
            AddTrack(T);
        end;

        // Carrega a capa
        FCoverSourceFile := '';
        if Assigned(FCover) then
           FCover.Free;
        if FindFirst(PlsPath + PathDelim + '*.bmp', faAnyFile, SR) = 0 then
           repeat
                 try
                    S := PlsPath + PathDelim + SR.Name;
                    FCover := TBitmap.Create;
                    FCover.LoadFromFile(S);
                    FCoverSourceFile := S;
                    Break;
                 except
                    FreeAndNil(FCover);
                 end;
           until FindNext(SR) <> 0;

        Result := FCoverSourceFile <> '';
     finally
        pls.Free;
     end;
end;

function TTitle.GetTracks: TStringList;
begin
     if not Assigned(FTracks) then
        InitTracks;
     Result := FTracks;
end;

procedure TTitle.RemoveFiles;
var T: TTrack;
    I: Integer;
begin
     // Deleta as músicas
     if not Assigned(FTracks) then
        InitTracks;
     for I := 0 to FTracks.Count-1 do begin
         T := FTracks.Objects[I] as TTrack;
         DeleteFile(T.TrackPath);
     end;

     // Deleta a capa
     DeleteFile(GetFilePath(FCoverPath));

     // Deleta o cds com as faixas
     DeleteFile(GetFilePath(FTracksPath));
end;

{ TTrack }

function TTrack.GetArtistName: String;
begin
     Result := FTitle.ArtistName;
end;

function TTrack.GetCaption: String;
begin
     Result := FormatFloat('00 ', FTrackId) + FTrackName;
end;

function TTrack.GetTrackPath: String;
begin
     if (FSourcePath = '') then
        Result := GetFilePath(FTrackPath)
     else
        Result := FSourcePath;
end;

function TTrack.Import: Boolean;
begin
     FTrackPath := HashCode(FTrackName);
     while FileExists(GetFilePath(FTrackPath)) do
           Inc(FTrackPath);
     Result := CopyFile(FSourcePath, GetFilePath(FTrackPath));
     if (Result) then
        FSourcePath := '';
end;

procedure TTrack.SetTrackPath(Value: String);
begin
     FSourcePath := Value;
end;

{ TKKMasterCD }

constructor TKKMasterCD.Create;
begin
     inherited Create;
     FTitles := TStringList.Create;
     FTitles.Sorted := False;
end;

destructor TKKMasterCD.Destroy;
var I: Integer;
begin
  for I := 0 to FTitles.Count-1 do
      FTitles.Objects[I].Free;
  FTitles.Free;
  inherited;
end;

function TKKMasterCD.Load: Boolean;
var SRArtist: TSearchRec;
    SRTitle: TSearchRec;
    T: TTitle;
begin
  if FindFirst(CD_ROOT + '*.*', faDirectory, SRArtist) = 0 then
     repeat
       if (SRArtist.Name[1] <> '.') then begin
          if FindFirst(CD_ROOT + SRArtist.Name + PathDelim + '*.*', faDirectory, SRTitle) = 0 then
            repeat
              if (SRTitle.Name[1] <> '.') then begin
                  T := TTitle.Create;
                  if not T.LoadFromPlaylist(CD_ROOT + SRArtist.Name + PathDelim + SRTitle.Name) then
                     T.Free
                  else
                     FTitles.AddObject(T.Caption, T);
              end;
            until FindNext(SRTitle) <> 0;
       end;
     until FindNext(SRArtist) <> 0;
  FTitles.Sort;
  Result := FTitles.Count > 0;
end;

end.
