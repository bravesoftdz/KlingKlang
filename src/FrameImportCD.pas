unit FrameImportCD;

interface

uses
  SysUtils, Types, Classes, Variants, QTypes, QGraphics, QControls, QForms, 
  QDialogs, QStdCtrls, FrameBase, QCheckLst, QExtCtrls, DataModule;

type
  TfrmeImportCD = class(TfrmeBase)
    lblAvailTitles: TLabel;
    lbxTitles: TCheckListBox;
    Label3: TLabel;
    Label4: TLabel;
    btnAll: TButton;
    btnNone: TButton;
    btnCopy: TButton;
    btnClose: TButton;
    lblAvailable: TLabel;
    lblSelected: TLabel;
    procedure lbxTitlesEnter(Sender: TObject);
    procedure lbxTitlesExit(Sender: TObject);
    procedure btnAllClick(Sender: TObject);
    procedure btnNoneClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure lbxTitlesClickCheck(Sender: TObject);
    procedure btnCopyClick(Sender: TObject);
  private
    { Private declarations }
    FCD: TKKMasterCD;
    function LoadCDContents: Boolean;
    procedure UpdateInfo;
  public
    { Public declarations }
    procedure Activate; override;
    procedure Deactivate; override;
    procedure ProcessKey(Key: Char); override;
  end;

var
  frmeImportCD: TfrmeImportCD;

implementation

uses KKUtils, FrameMenuOperator, FormProgress;

{$R *.xfm}

{ TfrmeImportCD }

procedure TfrmeImportCD.Activate;
begin
  inherited;
  Application.MainForm.ActiveControl := lbxTitles;
  if not LoadCDContents then
     btnCloseClick(btnClose);

  UpdateInfo;

  // Mutreta para não iniciar com o primeiro marcado:
  if lbxTitles.Items.Count > 0 then begin
     lbxTitles.ItemIndex := 0;
     lbxTitles.State[0] := cbChecked;
  end;
end;

procedure TfrmeImportCD.Deactivate;
begin
  inherited;
  if Assigned(FCD) then
     FreeAndNil(FCD);
end;

procedure TfrmeImportCD.ProcessKey(Key: Char);
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

procedure TfrmeImportCD.lbxTitlesEnter(Sender: TObject);
begin
  lbxTitles.Selected[0] := True;
end;

procedure TfrmeImportCD.lbxTitlesExit(Sender: TObject);
begin
  lbxTitles.Selected[lbxTitles.ItemIndex] := False;
end;

procedure TfrmeImportCD.lbxTitlesClickCheck(Sender: TObject);
begin
     UpdateInfo;
end;

procedure TfrmeImportCD.btnAllClick(Sender: TObject);
var I: Integer;
begin
  for I := 0 to lbxTitles.Items.Count-1 do
      lbxTitles.State[I] := cbChecked;
  lbxTitlesClickCheck(lbxTitles);
end;

procedure TfrmeImportCD.btnNoneClick(Sender: TObject);
var I: Integer;
begin
  for I := 0 to lbxTitles.Items.Count-1 do
      lbxTitles.State[I] := cbUnchecked;
  lbxTitlesClickCheck(lbxTitles);
end;

procedure TfrmeImportCD.btnCloseClick(Sender: TObject);
begin
  frmeMenuOperator.Activate;
end;

function TfrmeImportCD.LoadCDContents: Boolean;
var Loaded: Boolean;
begin
  Result := False;
  if Assigned(FCD) then
     FreeAndNil(FCD);
  FCD := TKKMasterCD.Create;
  repeat
        Loaded := FCD.Load;
        if not Loaded then
           Result := MessageDlg('', 'Insira o CD Master e aperte o botão [OK]', mtInformation, [mbOk, mbCancel], 0) = mrOk;
  until Loaded or not Result;
  if Loaded then begin
     lblAvailTitles.Caption := IntToStr(FCD.Titles.Count) + ' titúlos disponíveis no CD:';
     Result := True;
     lbxTitles.Clear;
     lbxTitles.Items.AddStrings(FCD.Titles);
  end;
end;

procedure TfrmeImportCD.btnCopyClick(Sender: TObject);
var I, Count: Integer;
begin
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
            frmProgress.lblInfo.Caption := 'Importando "' +
                                        lbxTitles.Items[I] + '"';
            frmProgress.pgbProgress.StepIt;
            frmProgress.Update;
            (FCD.Titles.Objects[I] as TTitle).Import;
            lbxTitles.State[I] := cbUnchecked;
            UpdateInfo;
         end;
     end;
  finally
     frmProgress.lblInfo.Caption := 'Importação Concluída';
     frmProgress.pgbProgress.StepIt;
     frmProgress.Update;
     Sleep(2000);
     frmProgress.Hide;
     Application.MainForm.Show;
  end;
end;

procedure TfrmeImportCD.UpdateInfo;
var TotalSpace, FreeSpace: Double;
    I: Integer;
    Size: Double;
begin
  Size := 0;
  for I := 0 to lbxTitles.Items.Count-1 do
      if lbxTitles.State[I] = cbChecked then
         Size := Size + (lbxTitles.Items.Objects[I] as TTitle).Size;

  CheckFreeSpace(ExtractFilePath(ParamStr(0)), TotalSpace, FreeSpace);
  lblAvailable.Caption := FormatDiskSpace(FreeSpace);
  lblSelected.Caption := FormatDiskSpace(Size);
  lblAvailable.Update;
  lblSelected.Update;
end;

end.
