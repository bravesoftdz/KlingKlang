unit FormMain;

interface

uses
  SysUtils, Types, Classes, Variants, QTypes, QGraphics, QControls, QForms,
  QT, QDialogs, QStdCtrls, QExtCtrls, CLXLabels, FrameBase;

type
  TfrmMain = class(TForm)
    Panel1: TPanel;
    slbCurrent: TScrollingLabel;
    Label1: TLabel;
    lcdCredits: TLCDNumber;
    lblQueue: TLabel;
    mmoQueue: TMemo;
    pnlMain: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure UpdatePlayInfo(Sender: TObject);
  private
    { Private declarations }
    FMode: Integer;
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

uses FrameSelector, FrameImportCD, FrameMenuOperator, FrameTitleManager,
  FrameMenuControl, KKUtils, FrameTitleView, FormPlayer, DataModule;

const
  MODE_NORMAL = 0;
  MODE_CONTROL = 1;
  MODE_OPERADOR = 2;

{$R *.xfm}

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
begin
     Screen.Cursor := crNone;
     FMode := MODE_NORMAL;
     frmeTitleManager := TfrmeTitleManager.Create(pnlMain);
     frmeMenuOperator := TfrmeMenuOperator.Create(pnlMain);
     frmeImportCD := TfrmeImportCD.Create(pnlMain);
     frmeSelector := TfrmeSelector.Create(pnlMain);
     frmeMenuControl := TfrmeMenuControl.Create(pnlMain);
     frmeTitleView := TfrmeTitleView.Create(pnlMain);
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
     frmeSelector.Activate;
     frmPlayer.OnUpdateInfo := UpdatePlayInfo;
     frmPlayer.Show;
     UpdatePlayInfo(Self);
end;

procedure TfrmMain.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var NewMode: Integer;
begin
     NewMode := FMode;
     case Key of
          Word('5'):  begin
                           KKData.AddCredit(False);
                           KKData.AddCredit(False);
                           UpdatePlayInfo(Self);
                           frmPlayer.PlayFile('coin');
                      end;
          Key_Return: ActiveFrame.ProcessKey(KKEY_OK);
          Key_Escape: ActiveFrame.ProcessKey(KKEY_CANCEL);
          Key_Up,
          Key_Left,
          Word('-'):  ActiveFrame.ProcessKey(KKEY_MINUS);
          Key_Down,
          Key_Right,
          Word('+'),
          Word('='):  ActiveFrame.ProcessKey(KKEY_PLUS);

          Key_F1: if (FMode = MODE_NORMAL) then
                     NewMode := MODE_CONTROL
                  else
                     NewMode := MODE_NORMAL;
          Key_F2: if (FMode = MODE_NORMAL) then
                     NewMode := MODE_OPERADOR
                  else
                     NewMode := MODE_NORMAL;
     end;
     if NewMode <> FMode then begin
        FMode := NewMode;
        case FMode of
             MODE_CONTROL:  frmeMenuControl.Activate;
             MODE_OPERADOR: frmeMenuOperator.Activate;
             MODE_NORMAL:   frmeSelector.Activate;
        end;
     end;
     Key := 0;
end;

procedure TfrmMain.UpdatePlayInfo(Sender: TObject);
var S: String;
begin
     mmoQueue.Lines.Clear;
     lblQueue.Caption := 'Nenhuma música em espera';
     if Assigned(frmPlayer.NowPlaying) then begin
        slbCurrent.Text := frmPlayer.NowPlaying.ArtistName +
                           ' - ' + frmPlayer.NowPlaying.TrackName;
        if frmPlayer.Queue.Count > 0 then begin
           lblQueue.Caption := 'Em espera: (' + IntToStr(frmPlayer.Queue.Count);
           if (frmPlayer.Queue.Count = 1) then
              S := ' música)'
           else
              S := ' músicas)';
           lblQueue.Caption := lblQueue.Caption + S;
           mmoQueue.Lines.AddStrings(frmPlayer.Queue);
        end;
     end
     else if KKData.Credits > 0 then
        slbCurrent.Text := 'Escolha sua música'
     else
        slbCurrent.Text := 'Insira créditos';
     slbCurrent.Text := slbCurrent.Text + '      ';
     lcdCredits.Value := IntToStr(KKData.Credits);
end;

end.
