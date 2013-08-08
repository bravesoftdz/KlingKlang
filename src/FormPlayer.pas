unit FormPlayer;

interface

uses
  SysUtils, Types, Classes, Variants, QTypes, QGraphics, QControls, QForms,
  QDialogs, QStdCtrls, QExtCtrls, ACS_Classes, ACS_Audio, ACS_Vorbis,
  QComCtrls, DataModule;

type
  TfrmPlayer = class(TForm)
    Label1: TLabel;
    Label3: TLabel;
    TrackBar1: TTrackBar;
    VorbisIn1: TVorbisIn;
    AudioOut1: TAudioOut;
    VorbisIn2: TVorbisIn;
    AudioOut2: TAudioOut;
    tmrBruteForce: TTimer;
    Label4: TLabel;
    AudioOut3: TAudioOut;
    VorbisIn3: TVorbisIn;
    procedure AudioOut1Progress(Sender: TComponent);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure AudioOut1Done(Sender: TComponent);
    procedure tmrBruteForceTimer(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormHide(Sender: TObject);
  private
    { Private declarations }
    FQueue: TStringList;
    FActiveOut: TAudioOut;
    FUpdateInfo: TNotifyEvent;
    FNowPlaying: TTrack;
    FVolume: Integer;
    procedure Play(Track: TTrack);
    function GetPlaying: Boolean;
    procedure SetVolume(const Value: Integer);
  public
    { Public declarations }
    procedure Enqueue(Track: TTrack);
    procedure Stop;
    function PlayFile(FileName: String): Boolean;
    property Queue: TStringList read FQueue;
    property NowPlaying: TTrack read FNowPlaying;
    property Playing: Boolean read GetPlaying;
    property OnUpdateInfo: TNotifyEvent read FUpdateInfo write FUpdateInfo;
    property Volume: Integer read FVolume write SetVolume;
  end;

var
  frmPlayer: TfrmPlayer;

implementation

{$R *.xfm}

{ TfrmPlayer }

procedure TfrmPlayer.FormCreate(Sender: TObject);
begin
     FQueue := TStringList.Create;
     AudioOut1.Delay := 6;
     AudioOut2.Delay := 6;
     AudioOut3.Delay := 6;
     FActiveOut := AudioOut2;
     Volume := KKData.Volume;
     KKData.LoadQueue(FQueue);
     if FQueue.Count > 0 then begin
        Play(FQueue.Objects[0] as TTrack);
        FQueue.Delete(0);
     end;
end;

procedure TfrmPlayer.FormDestroy(Sender: TObject);
begin
     FQueue.Free;
end;

procedure TfrmPlayer.Play(Track: TTrack);
var Min, Sec : Integer;
    Fmt : String;
    FI : TACSFileIn;
begin
     FNowPlaying := Track;
     FActiveOut.ThreadPriority := tpNormal;
     if FActiveOut = AudioOut1 then begin
        FActiveOut := AudioOut2;
        FI := VorbisIn2;
     end
     else begin
        FActiveOut := AudioOut1;
        FI := VorbisIn1;
     end;
     FActiveOut.ThreadPriority := tpHigher;
     FI.FileName := Track.TrackPath;
     if not FI.Valid then Exit;
     Sec := FI.TotalTime;
     Min := Sec div 60;
     Sec := Sec - Min*60;
     if Sec < 10 then Fmt := '%d:0%d'
     else Fmt := '%d:%d';
     Label1.Caption := Format(Fmt, [Min, Sec]);
     Label4.Caption := Track.TrackName;
     FActiveOut.Run;
end;

procedure TfrmPlayer.Enqueue(Track: TTrack);
begin
     KKData.AddToQueue(Track);
     if not Playing then begin
        Play(Track);
     end
     else
        FQueue.AddObject(Track.TrackName, Track);
     if Assigned(FUpdateInfo) then
        FUpdateInfo(Self);
     tmrBruteForce.Enabled := True;
end;

procedure TfrmPlayer.Stop;
begin
  tmrBruteForce.Enabled := False;
  AudioOut1.Stop;
  AudioOut2.Stop;
end;

function TfrmPlayer.GetPlaying: Boolean;
begin
     Result := (AudioOut1.Status = tosPlaying) or
               (AudioOut2.Status = tosPlaying);
end;

procedure TfrmPlayer.AudioOut1Progress(Sender: TComponent);
var Min, Sec : Integer;
    Fmt : String;
begin
     with (Sender as TAudioOut) do begin
          TrackBar1.Position := Progress;
          Sec := TimeElapsed;
     end;
     Min := Sec div 60;
     Sec := Sec - Min*60;
     if Sec < 10 then Fmt := '%d:0%d'
     else Fmt := '%d:%d';
     Label3.Caption := Format(Fmt, [Min, Sec]);
end;

procedure TfrmPlayer.AudioOut1Done(Sender: TComponent);
var Track: TTrack;
begin
     KKData.TrackCompleted;
     tmrBruteForce.Enabled := False;
     FNowPlaying := Nil;
     TrackBar1.Position := 0;
     if FQueue.Count > 0 then begin
        Track := FQueue.Objects[0] as TTrack;
        FQueue.Delete(0);
        Play(Track);
     end;
     tmrBruteForce.Enabled := True;
     if Assigned(FUpdateInfo) then
        FUpdateInfo(Self);
end;

procedure TfrmPlayer.tmrBruteForceTimer(Sender: TObject);
begin
     if (not Playing) and (Assigned(FNowPlaying)) then
        FActiveOut.Run;
end;

procedure TfrmPlayer.SetVolume(const Value: Integer);
begin
  FVolume := Value;
  AudioOut1.Volume := Value;
  AudioOut2.Volume := Value;
  KKData.Volume := Value;
end;

procedure TfrmPlayer.FormShow(Sender: TObject);
begin
     AudioOut1.OnProgress := AudioOut1Progress;
     AudioOut2.OnProgress := AudioOut1Progress;
end;

procedure TfrmPlayer.FormHide(Sender: TObject);
begin
     AudioOut1.OnProgress := Nil;
     AudioOut2.OnProgress := Nil;
end;

function TfrmPlayer.PlayFile(FileName: String): Boolean;
begin
     try
        VorbisIn3.FileName := FileName;
        if not VorbisIn3.Valid then
           Abort;
        AudioOut3.Run;
        Result := True;
     except
        Result := False;
     end;
end;

end.
