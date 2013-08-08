unit FrameMenuOperator;

interface

uses
  SysUtils, Types, Classes, Variants, QTypes, QGraphics, QControls, QForms, 
  QDialogs, QStdCtrls, FrameBase, QExtCtrls;

type
  TfrmeMenuOperator = class(TfrmeBase)
    btnImport: TButton;
    btnBalance: TButton;
    btnManage: TButton;
    procedure btnBalanceClick(Sender: TObject);
    procedure btnImportClick(Sender: TObject);
    procedure btnManageClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure Activate; override;
  end;

var
  frmeMenuOperator: TfrmeMenuOperator;

implementation

uses FrameImportCD, FrameTitleManager;

{$R *.xfm}

{ TfrmeMenuOperator }

procedure TfrmeMenuOperator.Activate;
begin
  inherited;
  Application.MainForm.ActiveControl := btnBalance;
end;

procedure TfrmeMenuOperator.btnBalanceClick(Sender: TObject);
begin
  inherited;
  ShowMessage((Sender as TButton).Caption);
end;

procedure TfrmeMenuOperator.btnImportClick(Sender: TObject);
begin
  inherited;
  frmeImportCD.Activate;
end;

procedure TfrmeMenuOperator.btnManageClick(Sender: TObject);
begin
  inherited;
  frmeTitleManager.Activate;
end;

end.
