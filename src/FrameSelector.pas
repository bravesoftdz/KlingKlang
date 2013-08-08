unit FrameSelector;

interface

uses
  SysUtils, Types, Classes, Variants, QTypes, QGraphics, QControls, QForms, 
  QDialogs, QStdCtrls, FrameBase, FrameCover, QExtCtrls;

type
  TfrmeSelector = class(TfrmeBase)
    frmeCover1: TfrmeCover;
    frmeCover2: TfrmeCover;
    frmeCover3: TfrmeCover;
    lblArtist: TLabel;
    lblTitle: TLabel;
    lblInstructions: TLabel;
    frmeCover4: TfrmeCover;
    frmeCover5: TfrmeCover;
    frmeCover6: TfrmeCover;
    frmeCover7: TfrmeCover;
    frmeCover8: TfrmeCover;
    frmeCover9: TfrmeCover;
    frmeCover10: TfrmeCover;
    frmeCover11: TfrmeCover;
    frmeCover12: TfrmeCover;
    tmrBlink: TTimer;
    imgKlingKlang: TImage;
    procedure tmrBlinkTimer(Sender: TObject);
  private
    { Private declarations }
    FSelected: TfrmeCover;
    FPage: Integer;
    FIndex: Integer;
    FUpdate: TDateTime;
    procedure UpdateCovers;
    procedure NextPage;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    procedure Activate; override;
    procedure ProcessKey(Key: Char); override;
  end;

var
  frmeSelector: TfrmeSelector;

implementation

uses DataModule, KKUtils, FrameTitleView;

{$R *.xfm}

constructor TfrmeSelector.Create(AOwner: TComponent);
begin
  inherited;
  FUpdate := 0;
end;

procedure TfrmeSelector.Activate;
begin
  inherited;
  if Assigned(FSelected) then
     FSelected.Selected := False;
  if KKData.LastUpdate <> FUpdate then begin
     FUpdate := KKData.LastUpdate;
     FSelected := Nil;
     FIndex := 0;
     FPage := -1;
     UpdateCovers;
  end;
end;

procedure TfrmeSelector.tmrBlinkTimer(Sender: TObject);
begin
  inherited;
  if Assigned(FSelected) then
     FSelected.Selected := not FSelected.Selected;
end;

procedure TfrmeSelector.ProcessKey(Key: Char);
begin
     if (KKData.Titles.Count > 0) then begin
         case Key of
              KKEY_OK:     begin
                                frmeTitleView.Title := FSelected.Title;
                                frmeTitleView.Activate;
                           end;
              KKEY_MINUS:  if (FIndex = 0) then
                              FIndex := KKData.Titles.Count-1
                           else
                              Dec(FIndex);
              KKEY_PLUS:   if (FIndex = KKData.Titles.Count-1) then
                              FIndex := 0
                           else
                              Inc(FIndex);
              KKEY_CANCEL: NextPage;
         end;
         UpdateCovers;
     end;
end;

procedure TfrmeSelector.NextPage;
var P, I: Integer;
begin
     P := FPage;
     I := FIndex;
     while (I < KKData.Titles.Count-1) do begin
           Inc(I);
           P := (I div 12) + 1;
           if (P <> FPage) then
              Break;
     end;

     if (P <> FPage) then 
        FIndex := I
     else
        FIndex := 0;
end;

procedure TfrmeSelector.UpdateCovers;
var NewFrame, AFrame: TfrmeCover;
    NewPage: Integer;
    I, Initial: Integer;
begin
     tmrBlink.Enabled := False;
     try
        if (KKData.Titles.Count > 0) then begin
           NewPage := (FIndex div 12) + 1;
           NewFrame := FindComponent('frmeCover' + IntToStr(FIndex mod 12 + 1)) as TfrmeCover;
           if NewPage <> FPage then begin
              Initial := (FIndex div 12) * 12;
              I := Initial;
              while ((I - Initial) < 12) do begin
                    AFrame := FindComponent('frmeCover' + IntToStr((I - Initial) + 1)) as TfrmeCover;
                    AFrame.Visible := True;
                    if (I < KKData.Titles.Count) then
                       AFrame.Title := (KKData.Titles.Objects[I] as TTitle)
                    else
                       AFrame.Clear;
                    Inc(I);
              end;
              FPage := NewPage;
           end;

           if NewFrame <> FSelected then begin
              if Assigned(FSelected) then
                 FSelected.Selected := False;
              NewFrame.Selected := True;
              FSelected := NewFrame;
              lblArtist.Caption := FSelected.ArtistName;
              lblTitle.Caption := FSelected.TitleName;
           end;
           lblInstructions.Caption := 'Pressione o botão [OK] para escolher sua música';
           imgKlingKlang.Visible := False;
        end
        else begin
           lblArtist.Caption := 'Nenhum título encontrado!';
           lblTitle.Caption := 'Por favor, entre em contato com o nosso representante.';
           for I := 1 to 12 do begin
               AFrame := FindComponent('frmeCover' + IntToStr(I)) as TfrmeCover;
               AFrame.Clear;
               AFrame.Visible := False;
           end;
           lblInstructions.Caption := 'klingklang@medialabs.com.br';
           imgKlingKlang.Visible := True;
           imgKlingKlang.BringToFront;
        end;
     finally
        tmrBlink.Enabled := True;
     end;
end;

end.
