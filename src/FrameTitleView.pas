unit FrameTitleView;

interface

uses
  SysUtils, Types, Classes, Variants, QTypes, QGraphics, QControls, QForms,
  QDialogs, QStdCtrls, FrameBase, QExtCtrls, DataModule, CLXLabels;

type
  TfrmeTitleView = class(TfrmeBase)
    lblTitle: TLabel;
    lbxTracks: TListBox;
    lblCredits: TLabel;
    mmoDescription: TMemo;
    Label4: TLabel;
    edtYear: TEdit;
    Label3: TLabel;
    Panel1: TPanel;
    imgCover: TImage;
    mmoTracks: TMemo;
    slbTrackName: TScrollingLabel;
  private
    { Private declarations }
    FTitle: TTitle;
    FIndex: Integer;
    procedure SetTitle(const Value: TTitle);
    procedure UpdateCursor;
  public
    { Public declarations }
    procedure Activate; override;
    procedure ProcessKey(Key: Char); override;
    property Title: TTitle read FTitle write SetTitle;
  end;

var
  frmeTitleView: TfrmeTitleView;

implementation

uses FrameSelector, KKUtils, FormPlayer;

{$R *.xfm}

{ TfrmeTitleView }

procedure TfrmeTitleView.Activate;
begin
  inherited;
  slbTrackName.Width := mmoTracks.Width-6;
  slbTrackName.Left := mmoTracks.Left+4;
  FIndex := 0;
  UpdateCursor;
end;

procedure TfrmeTitleView.ProcessKey(Key: Char);
var Last: Integer;
begin
     Last := FIndex;
     case Key of
          KKEY_OK:     if KKData.Credits > 0 then begin
                          KKData.UseCredit;
                          frmPlayer.Enqueue(FTitle.Tracks[FIndex]);
                          frmeSelector.Activate;
                       end;
          KKEY_CANCEL: frmeSelector.Activate;
          KKEY_MINUS:  if (FIndex > 0) then
                          Dec(FIndex);
          KKEY_PLUS:   if (FIndex < FTitle.TrackCount-1) then
                          Inc(FIndex);
     end;
     if (Last <> FIndex) then
        UpdateCursor;
end;

procedure TfrmeTitleView.SetTitle(const Value: TTitle);
begin
  FTitle := Value;
  Label1.Caption := FTitle.ArtistName;
  lblTitle.Caption := FTitle.TitleName;
  imgCover.Picture.Assign(FTitle.Cover);
  edtYear.Text := IntToStr(FTitle.Year);
  mmoDescription.Lines.Text := FTitle.Description;
  mmoTracks.Lines.Assign(FTitle.TrackList);
end;

procedure TfrmeTitleView.UpdateCursor;
begin
     slbTrackName.Visible := False;
     slbTrackName.Top := mmoTracks.Top + 2 + (mmoTracks.Font.Height + 2) * FIndex;
     slbTrackName.Text := mmoTracks.Lines[FIndex];
     slbTrackName.Visible := True;
     slbTrackName.Scrolling := (slbTrackName.TextWidth >= mmoTracks.Width-8);
     if slbTrackName.Scrolling then
        slbTrackName.Text := slbTrackName.Text + '             ';
end;

end.
