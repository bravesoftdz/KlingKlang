unit FormTitle;

interface

uses
  SysUtils, Types, Classes, Variants, QTypes, QGraphics, QControls, QForms,
  QDialogs, QStdCtrls, QExtCtrls, QT, DM;

type
  TSelectTrackEvent = procedure (TrackPath: String) of object;

type
  TfrmTitle = class(TForm)
    imgCover: TImage;
    Label1: TLabel;
    edtTitle: TEdit;
    lbxTracks: TListBox;
    Label2: TLabel;
    edtArtist: TEdit;
    Label3: TLabel;
    edtYear: TEdit;
    Label4: TLabel;
    mmoDescription: TMemo;
    lblCredits: TLabel;
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormShow(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }
    FTitle: TTitle;
    FSelectTrack: TSelectTrackEvent;
  public
    { Public declarations }
    procedure ShowTitle(Title: TTitle);
    property OnSelectTrack: TSelectTrackEvent read FSelectTrack write FSelectTrack;
  end;

var
  frmTitle: TfrmTitle;

implementation

{$R *.xfm}

{ TfrmTitle }

procedure TfrmTitle.ShowTitle(Title: TTitle);
var I: Integer;
begin
     FTitle := Title;
     edtTitle.Text := Title.TitleName;
     edtArtist.Text := Title.ArtistName;
     edtYear.Text := IntToStr(Title.Year);
     mmoDescription.Lines.Text := Title.Description;
     imgCover.Picture.Assign(Title.Cover);
     lbxTracks.Items.Clear;
     for I := 0 to Title.Tracks.Count-1 do
         lbxTracks.Items.Add(Title.Tracks.Names[I]);
     ShowModal;
end;

procedure TfrmTitle.FormKeyPress(Sender: TObject; var Key: Char);
begin
     case Key of
          #27: ModalResult := mrCancel;
          #13: if Assigned(FSelectTrack) then
                  FSelectTrack(FTitle.Tracks[lbxTracks.ItemIndex]);
     end;
end;

procedure TfrmTitle.FormShow(Sender: TObject);
begin
     Screen.Cursor := crNone;
     lbxTracks.ItemIndex := 0;
end;

procedure TfrmTitle.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
     case Key of
          Key_Left: if (lbxTracks.ItemIndex > 0) then
                       lbxTracks.ItemIndex := lbxTracks.ItemIndex - 1;
          Key_Right: lbxTracks.ItemIndex := lbxTracks.ItemIndex + 1;
     end;
end;

end.
