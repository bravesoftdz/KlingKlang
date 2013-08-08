unit FormPlayer;

interface

uses
  SysUtils, Types, Classes, Variants, QTypes, QGraphics, QControls, QForms,
  QDialogs, QStdCtrls, ACS_Classes, ACS_Audio, ACS_Vorbis, QComCtrls,
  QExtCtrls;

type
  TfrmPlayer = class(TForm)
    VorbisIn1: TVorbisIn;
    AudioOut1: TAudioOut;
    VorbisIn2: TVorbisIn;
    AudioOut2: TAudioOut;
    Label2: TLabel;
    Label1: TLabel;
    Label3: TLabel;
    TrackBar1: TTrackBar;
    tmrBruteForce: TTimer;
    procedure AudioOut1Progress(Sender: TComponent);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure AudioOut1Done(Sender: TComponent);
    procedure tmrBruteForceTimer(Sender: TObject);
  private
    { Private declarations }
    FQueue: TStringList;
    FActiveOut: TAudioOut;
    FUpdateInfo: TNotifyEvent;
    FNowPlaying: String;
    procedure Play(Track: String);
    function GetPlaying: Boolean;
  public
    { Public declarations }
    procedure Enqueue(Track: String);
    procedure Stop;
    property Queue: TStringList read FQueue;
    property Playing: Boolean read GetPlaying;
    property OnUpdateInfo: TNotifyEvent read FUpdateInfo write FUpdateInfo;
  end;

var
  frmPlayer: TfrmPlayer;

implementation

{$R *.xfm}

{ TForm2 }

procedure TfrmPlayer.FormCreate(Sender: TObject);
begin
     FQueue := TStringList.Create;
     AudioOut1.Delay := 6;
     AudioOut2.Delay := 6;
     FActiveOut := AudioOut2;
end;

procedure TfrmPlayer.FormDestroy(Sender: TObject);
begin
{
     if (AudioOut1.Status <> tosIdle) then
        AudioOut1.Stop;
     if (AudioOut2.Status <> tosIdle) then
        AudioOut2.Stop;
}
     FQueue.Free;
end;

procedure TfrmPlayer.Play(Track: String);
var Min, Sec : Integer;
    Fmt : String;
    FileName: String;
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
     FileName := Copy(Track, Pos('=', Track)+1, 400);
     FI.FileName := FileName;
     if not FI.Valid then Exit;
     Sec := FI.TotalTime;
     Min := Sec div 60;
     Sec := Sec - Min*60;
     if Sec < 10 then Fmt := '%d:0%d'
     else Fmt := '%d:%d';
     Label1.Caption := Format(Fmt, [Min, Sec]);
     Label2.Caption := ExtractFileName(FileName);
     FActiveOut.Run;
end;

procedure TfrmPlayer.Enqueue(Track: String);
begin
     Delete(Track, 1, 3);
     if not Playing then begin
        if Assigned(FUpdateInfo) then
           FUpdateInfo(Self);
        Play(Track);
     end
     else
        FQueue.Add(Track);
end;

procedure TfrmPlayer.Stop;
begin
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
          FNowPlaying := (Input as TVorbisIn).FileName;
     end;
     Min := Sec div 60;
     Sec := Sec - Min*60;
     if Sec < 10 then Fmt := '%d:0%d'
     else Fmt := '%d:%d';
     Label3.Caption := Format(Fmt, [Min, Sec]);
end;

procedure TfrmPlayer.AudioOut1Done(Sender: TComponent);
var Track: String;
begin
     tmrBruteForce.Enabled := False;
     FNowPlaying := '';
     TrackBar1.Position := 0;
     if FQueue.Count > 0 then begin
        Track := FQueue[0];
        FQueue.Delete(0);
        Play(Track);
        if Assigned(FUpdateInfo) then
           FUpdateInfo(Self);
     end;
     tmrBruteForce.Enabled := True;
end;

procedure TfrmPlayer.tmrBruteForceTimer(Sender: TObject);
begin
     if (not Playing) and (FNowPlaying <> '') then
        FActiveOut.Run;
end;

end.
