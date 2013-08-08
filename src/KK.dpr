program KK;

uses
  QForms,
  FormMain in 'FormMain.pas' {frmMain},
  DataModule in 'DataModule.pas' {KKData: TDataModule},
  FrameCover in 'FrameCover.pas' {frmeCover: TFrame},
  FrameBase in 'FrameBase.pas' {frmeBase: TFrame},
  FrameSelector in 'FrameSelector.pas' {frmeSelector: TFrame},
  FrameImportCD in 'FrameImportCD.pas' {frmeImportCD: TFrame},
  FrameTitleManager in 'FrameTitleManager.pas' {frmeTitleManager: TFrame},
  FrameMenuOperator in 'FrameMenuOperator.pas' {frmeMenuOperator: TFrame},
  FrameMenuControl in 'FrameMenuControl.pas' {frmeMenuControl: TFrame},
  KKUtils in 'KKUtils.pas',
  FormProgress in 'FormProgress.pas' {frmProgress},
  FrameTitleView in 'FrameTitleView.pas' {frmeTitleView: TFrame},
  FormPlayer in 'FormPlayer.pas' {frmPlayer};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Kling Klang - Music Machine';
  Application.CreateForm(TKKData, KKData);
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmProgress, frmProgress);
  Application.CreateForm(TfrmPlayer, frmPlayer);
  Application.Run;
end.
