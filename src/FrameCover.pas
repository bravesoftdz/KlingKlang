unit FrameCover;

interface

uses
  SysUtils, Types, Classes, Variants, QTypes, QGraphics, QControls, QForms,
  QExtCtrls, DataModule;

type
  TfrmeCover = class(TFrame)
    imgCover: TImage;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
  private
    { Private declarations }
    FSelected: Boolean;
    FTitle: TTitle;
    procedure SetSelected(const Value: Boolean);
    function GetArtistName: String;
    function GetTitleName: String;
    procedure SetTitle(T: TTitle);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    procedure Clear;
    property Selected: Boolean read FSelected write SetSelected;
    property Title: TTitle read FTitle write SetTitle;
    property TitleName: String read GetTitleName;
    property ArtistName: String read GetArtistName;
  end;

implementation

{$R *.xfm}

{ TfrmeCover }

var bmpEmpty: TBitmap;

procedure TfrmeCover.Clear;
begin
     if not Assigned(bmpEmpty) then begin
        bmpEmpty := TBitmap.Create;
        bmpEmpty.Width := imgCover.Width;
        bmpEmpty.Height := imgCover.Height;
        bmpEmpty.Canvas.Brush.Color := clBlue;
        bmpEmpty.Canvas.FillRect(imgCover.ClientRect);
     end;
     imgCover.Picture.Assign(bmpEmpty);
     Selected := False;
     FTitle := Nil;
end;

constructor TfrmeCover.Create(AOwner: TComponent);
begin
  inherited;
end;

function TfrmeCover.GetArtistName: String;
begin
     if Assigned(FTitle) then
        Result := FTitle.ArtistName
     else
        Result := '';
end;

function TfrmeCover.GetTitleName: String;
begin
     if Assigned(FTitle) then
        Result := FTitle.TitleName
     else
        Result := '';
end;

procedure TfrmeCover.SetSelected(const Value: Boolean);
begin
  FSelected := Value;
  if not FSelected then
     Panel1.Color := clBlue
  else begin
     Panel1.Color := clAqua;
     BringToFront;
  end;
  Panel2.Color := Panel1.Color;
  Panel3.Color := Panel1.Color;
  Panel4.Color := Panel1.Color;
end;

procedure TfrmeCover.SetTitle(T: TTitle);
begin
     if (T <> FTitle) then begin
        FTitle := T;
        if Assigned(T) then
           imgCover.Picture.Assign(T.Cover);
     end;
end;

end.
