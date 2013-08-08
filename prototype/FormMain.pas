unit FormMain;

interface

uses
  SysUtils, Types, Classes, Variants, QTypes, QGraphics, QControls, QForms,
  QT, QDialogs, QStdCtrls, QExtCtrls;

type
  TfrmMain = class(TForm)
    Image1: TImage;
    Image2: TImage;
    Image3: TImage;
    Image4: TImage;
    Image5: TImage;
    Image6: TImage;
    Image7: TImage;
    Image8: TImage;
    Image9: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    lblCredits: TLabel;
    lcdCredits: TLCDNumber;
    lbxQueue: TListBox;
    lblQueue: TLabel;
    tmrBlink: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure tmrBlinkTimer(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure UpdateInfo(Sender: TObject);
    procedure FormHide(Sender: TObject);
  protected
    function WidgetFlags: Integer; override;
  private
    { Private declarations }
    Credits: Integer;
    Cursor: Integer;
    SelectedTitle: Integer;
    procedure DrawCovers;
    procedure NextCover;
    procedure PrevCover;
    procedure SelectTrack(TrackPath: String);
    procedure UpdateQueue;
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

uses FormTitle, DM, FormPlayer;

{$R *.xfm}

procedure TfrmMain.FormCreate(Sender: TObject);
begin
     Left := 0;
     Top := 0;
     Cursor := 1;
     DrawCovers;
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
     Screen.Cursor := crNone;
     frmTitle.OnSelectTrack := SelectTrack;
     frmPlayer.OnUpdateInfo := UpdateInfo;
     frmPlayer.Show;
end;

procedure TfrmMain.DrawCovers;
var ALabel: TLabel;
    AImage: TImage;
    I: Integer;
    Title: TTitle;
begin
     for I := 1 to 9 do begin
         AImage := TImage(FindComponent('Image' + IntToStr(I)));
         ALabel := TLabel(FindComponent('Label' + IntToStr(I)));
         if (Cursor + I - 1 <= dtmKlingKlang.Titles.Count) then begin
            Title := (dtmKlingKlang.Titles.Objects[Cursor+I-2] as TTitle);
            AImage.Picture.Assign(Title.Cover);
            ALabel.Caption := Title.Caption;
         end
         else begin
            ALabel.Caption := '';
         end;
     end;
end;

procedure TfrmMain.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var SavedKey: Word;
begin
     SavedKey := Key;
     Key := 0;
     case SavedKey of
          Key_Left,
          Key_Up: PrevCover;
          Key_Right,
          Key_Down: NextCover;
          else Key := SavedKey;
     end;
end;

procedure TfrmMain.FormKeyPress(Sender: TObject; var Key: Char);
begin
     case Key of
          '=', '+': begin
                         Inc(Credits);
                         lcdCredits.Value := IntToStr(Credits);
                    end;
          #13: begin
                    frmTitle.Left := Left;
                    frmTitle.Top := Top;
                    frmTitle.ShowTitle(dtmKlingKlang.Titles.Objects[Cursor-1] as TTitle);
                    Show;
               end;
     end;
end;

procedure TfrmMain.NextCover;
begin
     Inc(Cursor);
     if (Cursor = 10) then
        Cursor := 1;
     tmrBlinkTimer(tmrBlink);
end;

procedure TfrmMain.PrevCover;
begin
     Dec(Cursor);
     if (Cursor = 0) then
        Cursor := 9;
     tmrBlinkTimer(tmrBlink);
end;

procedure TfrmMain.tmrBlinkTimer(Sender: TObject);
var ALabel: TLabel;
    I: Integer;
begin
     for I := 1 to 9 do begin
         ALabel := TLabel(FindComponent('Label' + IntToStr(I)));
         if (I <> Cursor) then begin
            ALabel.Color := Color;
            ALabel.Font.Color := clWhite;
         end
         else begin
            if (ALabel.Color = Color) then begin
               ALabel.Color := clWhite;
               ALabel.Font.Color := clBlack;
            end
            else begin
               ALabel.Color := Color;
               ALabel.Font.Color := clWhite;
            end;
         end;
     end;
end;

procedure TfrmMain.SelectTrack(TrackPath: String);
begin
     if Credits > 0 then begin
        frmPlayer.Enqueue(TrackPath);
        Dec(Credits);
        UpdateQueue;
     end;
end;

procedure TfrmMain.UpdateQueue;
var I: Integer;
begin
     lcdCredits.Value := IntToStr(Credits);
     lbxQueue.Items.Clear;
     for I := 0 to frmPlayer.Queue.Count-1 do
         lbxQueue.Items.Add(frmPlayer.Queue.Names[I]);
     I := frmPlayer.Queue.Count;
     lblQueue.Caption := 'Em espera: ';
     if I = 1 then
        lblQueue.Caption := lblQueue.Caption + '1 música'
     else if I > 1 then
        lblQueue.Caption := lblQueue.Caption + IntToStr(I) + ' músicas';
end;

procedure TfrmMain.UpdateInfo(Sender: TObject);
begin
     UpdateQueue;
end;

procedure TfrmMain.FormHide(Sender: TObject);
begin
     Halt;
end;

function TfrmMain.WidgetFlags: Integer;
begin
  // To reduce flickering on LINUX
  Result := Inherited WidgetFlags or
            Integer(WidgetFlags_WRepaintNoErase) or
            Integer(WidgetFlags_WResizeNoErase);
end;

end.
