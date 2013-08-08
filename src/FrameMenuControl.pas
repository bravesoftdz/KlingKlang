unit FrameMenuControl;

interface

uses
  SysUtils, Types, Classes, Variants, QTypes, QGraphics, QControls, QForms, 
  QDialogs, QStdCtrls, FrameBase, QComCtrls, QExtCtrls;

type
  TfrmeMenuControl = class(TfrmeBase)
    pgbVolume: TProgressBar;
    Label2: TLabel;
    Label4: TLabel;
  private
    { Private declarations }
    FOkCount: Integer;
  public
    { Public declarations }
    procedure Activate; override;
    procedure ProcessKey(Key: Char); override;
  end;

var
  frmeMenuControl: TfrmeMenuControl;

implementation

uses KKUtils, FormPlayer, FormMain, DataModule;

{$R *.xfm}

procedure TfrmeMenuControl.Activate;
begin
  inherited;
  pgbVolume.Position := frmPlayer.Volume;
  FOkCount := 0;
end;

procedure TfrmeMenuControl.ProcessKey(Key: Char);
var OldVolume: Integer;
begin
     OldVolume := pgbVolume.Position;
     case Key of
          KKEY_OK:     begin
                            Inc(FOkCount);
                            if FOkCount = 2 then begin
                               KKData.AddCredit(True);
                               frmMain.UpdatePlayInfo(Self);
                               frmPlayer.PlayFile('coin');
                               FOkCount := 0;
                            end;
                       end;
          KKEY_CANCEL: ;
          KKEY_MINUS:  pgbVolume.StepBy(-5);
          KKEY_PLUS:   pgbVolume.StepBy(+5);
     end;
     if OldVolume <> pgbVolume.Position then
        frmPlayer.Volume := pgbVolume.Position;
end;



end.
