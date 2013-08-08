unit FrameTitleManager;

interface

uses
  SysUtils, Types, Classes, Variants, QTypes, QGraphics, QControls, QForms, 
  QDialogs, QStdCtrls, FrameBase, QCheckLst;

type
  TfrmeTitleManager = class(TfrmeBase)
    lblAvailTitles: TLabel;
    lbxTitles: TCheckListBox;
    Label3: TLabel;
    Label4: TLabel;
    lblAvailable: TLabel;
    lblSelected: TLabel;
    btnHide: TButton;
    btnClose: TButton;
    btnShow: TButton;
    btnRemove: TButton;
    Label7: TLabel;
    Label6: TLabel;
    lblHidden: TLabel;
    procedure lbxTitlesEnter(Sender: TObject);
    procedure lbxTitlesExit(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure lbxTitlesClickCheck(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
    procedure btnHideClick(Sender: TObject);
    procedure btnShowClick(Sender: TObject);
  private
    { Private declarations }
    procedure LoadLibrary;
    procedure UpdateInfo;
  public
    { Public declarations }
    procedure Activate; override;
    procedure ProcessKey(Key: Char); override;
  end;

var
  frmeTitleManager: TfrmeTitleManager;

implementation

uses FrameMenuOperator, KKUtils, DataModule, FormProgress, FrameSelector;

{$R *.xfm}

{ TfrmeTitleManager }

procedure TfrmeTitleManager.Activate;
begin
  inherited;
  if KKData.Titles.Count + KKData.HiddenTitles.Count = 0 then begin
     btnCloseClick(btnClose);
     Exit;
  end;

  Application.MainForm.ActiveControl := lbxTitles;
  LoadLibrary;
  UpdateInfo;

  // Mutreta para não iniciar com o primeiro marcado:
  if lbxTitles.Items.Count > 0 then begin
     lbxTitles.ItemIndex := 0;
     lbxTitles.State[0] := cbChecked;
  end;
end;

procedure TfrmeTitleManager.ProcessKey(Key: Char);
var I: Integer;
begin
  if (Application.MainForm.ActiveControl = lbxTitles) then begin
     I := lbxTitles.ItemIndex;
     case Key of
          KKEY_PLUS:  lbxTitles.ItemIndex := I + 1;
          KKEY_MINUS: if (I > 0) then lbxTitles.ItemIndex := I - 1;
          KKEY_OK: begin
                        if lbxTitles.State[I] = cbUnchecked then
                           lbxTitles.State[I] := cbChecked
                        else
                           lbxTitles.State[I] := cbUnchecked;
                        lbxTitlesClickCheck(lbxTitles);
                   end;
          else inherited;
     end;
  end
  else begin
     inherited;
  end;
end;

procedure TfrmeTitleManager.lbxTitlesEnter(Sender: TObject);
begin
  lbxTitles.Selected[0] := True;
end;

procedure TfrmeTitleManager.lbxTitlesExit(Sender: TObject);
begin
  lbxTitles.Selected[lbxTitles.ItemIndex] := False;
end;

procedure TfrmeTitleManager.btnCloseClick(Sender: TObject);
begin
  frmeMenuOperator.Activate;
end;

procedure TfrmeTitleManager.lbxTitlesClickCheck(Sender: TObject);
begin
     UpdateInfo;
end;

procedure TfrmeTitleManager.LoadLibrary;
var I: Integer;
begin
     lbxTitles.Items.Assign(KKData.Titles);
     for I := 0 to KKData.HiddenTitles.Count-1 do
         lbxTitles.Items.AddObject('(*) ' + KKData.HiddenTitles[I],
                                  KKData.HiddenTitles.Objects[I]);
end;

procedure TfrmeTitleManager.btnRemoveClick(Sender: TObject);
var I, Count: Integer;
begin
  I := MessageDlg('Confirmação', 'Deseja realmente excluir os títulos selecionados?' + #13 +
                    'Esta operação não poderá ser desfeita!', mtConfirmation,
                    [mbOk, mbCancel], 0, mbCancel);
  if (I <> mrOk) then
     Exit;

  Count := 0;
  for I := 0 to lbxTitles.Items.Count - 1 do
      if lbxTitles.State[I] = cbChecked then
         Inc(Count);
  if Count = 0 then
     Exit;

  frmProgress.pgbProgress.Position := 0;
  frmProgress.pgbProgress.Max := Count+1;
  frmProgress.Show;
  try
     for I := 0 to lbxTitles.Items.Count - 1 do begin
         if lbxTitles.State[I] = cbChecked then begin
            frmProgress.lblInfo.Caption := 'Excluíndo "' +
                                        lbxTitles.Items[I] + '"';
            frmProgress.pgbProgress.StepIt;
            frmProgress.Update;
            KKData.RemoveTitle(lbxTitles.Items.Objects[I] as TTitle);
            lbxTitles.State[I] := cbUnchecked;
            UpdateInfo;
         end;
     end;
  finally
     frmProgress.lblInfo.Caption := 'Exclusão Concluída';
     frmProgress.pgbProgress.StepIt;
     frmProgress.Update;
     Sleep(2000);
     frmProgress.Hide;
     LoadLibrary;
     UpdateInfo;
     Application.MainForm.Show;
  end;
end;

procedure TfrmeTitleManager.UpdateInfo;
var TotalSpace, FreeSpace: Double;
    I: Integer;
    HSize, SSize: Double;
    S: String;
begin
  SSize := 0;
  for I := 0 to lbxTitles.Items.Count-1 do
      if lbxTitles.State[I] = cbChecked then
         SSize := SSize + (lbxTitles.Items.Objects[I] as TTitle).Size;

  HSize := 0;
  for I := 0 to KKData.HiddenTitles.Count-1 do
      HSize := HSize + (KKData.HiddenTitles.Objects[I] as TTitle).Size;

  CheckFreeSpace(ExtractFilePath(ParamStr(0)), TotalSpace, FreeSpace);
  lblAvailable.Caption := FormatDiskSpace(FreeSpace);
  lblAvailable.Update;
  lblSelected.Caption := FormatDiskSpace(SSize);
  lblSelected.Update;
  lblHidden.Caption := FormatDiskSpace(HSize);
  lblHidden.Update;
  I := KKData.HiddenTitles.Count;
  lblAvailTitles.Caption := IntToStr(KKData.Titles.Count + I) +
                            ' titúlos disponíveis na Máquina';
  case I of
       0: ;
       1: S := ' (1 escondido)';
       else S := ' (' + IntToStr(I) + ' escondidos)';
  end;
  lblAvailTitles.Caption := lblAvailTitles.Caption + S;
end;

procedure TfrmeTitleManager.btnHideClick(Sender: TObject);
var I: Integer;
begin
     for I := 0 to lbxTitles.Items.Count-1 do begin
         if (lbxTitles.State[I] = cbChecked) and (lbxTitles.Items.Objects[I] as TTitle).Visible then
            KKData.HideTitle(lbxTitles.Items.Objects[I] as TTitle);
     end;
     LoadLibrary;
     UpdateInfo;
end;

procedure TfrmeTitleManager.btnShowClick(Sender: TObject);
var I: Integer;
begin
     for I := 0 to lbxTitles.Items.Count-1 do begin
         if (lbxTitles.State[I] = cbChecked) and not (lbxTitles.Items.Objects[I] as TTitle).Visible then
            KKData.UnHideTitle(lbxTitles.Items.Objects[I] as TTitle);
     end;
     LoadLibrary;
     UpdateInfo;
end;

end.
