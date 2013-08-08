unit FormProgress;

interface

uses
  SysUtils, Types, Classes, Variants, QTypes, QGraphics, QControls, QForms, 
  QDialogs, QStdCtrls, QComCtrls, QExtCtrls;

type
  TfrmProgress = class(TForm)
    Panel1: TPanel;
    lblInfo: TLabel;
    pgbProgress: TProgressBar;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmProgress: TfrmProgress;

implementation

{$R *.xfm}

end.
