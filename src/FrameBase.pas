unit FrameBase;

interface

uses
  SysUtils, Types, Classes, Variants, QTypes, QGraphics, QControls, QForms,
  QStdCtrls;

type
  TfrmeBase = class(TFrame)
    Label1: TLabel;
    procedure ButtonEnter(Sender: TObject);
    procedure ButtonExit(Sender: TObject);
  private
    { Private declarations }
  protected
    FBlinkState: Boolean;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    procedure Activate; virtual;
    procedure Deactivate; virtual;
    procedure ProcessKey(Key: Char); virtual;
  end;

var
  ActiveFrame: TfrmeBase;

implementation

uses KKUtils;

{$R *.xfm}

{ TfrmeBase }

constructor TfrmeBase.Create(AOwner: TComponent);
begin
     inherited;
     Parent := (Owner as TWidgetControl);
     Visible := False;
end;

procedure TfrmeBase.Activate;
begin
     if Assigned(ActiveFrame) then
        ActiveFrame.Deactivate;
     Visible := True;
     ActiveFrame := Self;
end;

procedure TfrmeBase.Deactivate;
begin
     Visible := False;
end;

procedure TfrmeBase.ProcessKey(Key: Char);
var AControl: TWidgetControl;
begin
     AControl := Application.MainForm.ActiveControl;

     case Key of
          KKEY_OK:     ;
          KKEY_CANCEL: SelectNext(AControl, True, True);
          KKEY_MINUS:  SelectNext(AControl, False, True);
          KKEY_PLUS:   SelectNext(AControl, True, True);
     end;
end;

procedure TfrmeBase.ButtonEnter(Sender: TObject);
begin
     with (Sender as TButton) do begin
          Color := clAqua;
          Font.Color := clBlack;
     end;
end;

procedure TfrmeBase.ButtonExit(Sender: TObject);
begin
     with (Sender as TButton) do begin
          Color := clBlue;
          Font.Color := clWhite;
     end;
end;

end.
