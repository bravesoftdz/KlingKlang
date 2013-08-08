program KlingKlang;

uses
  QForms,
  DM in 'DM.pas' {dtmKlingKlang: TDataModule},
  FormMain in 'FormMain.pas' {frmMain},
  FormTitle in 'FormTitle.pas' {frmTitle},
  FormPlayer in 'FormPlayer.pas' {frmPlayer};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Kling Klang';
  Application.CreateForm(TdtmKlingKlang, dtmKlingKlang);
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmTitle, frmTitle);
  Application.CreateForm(TfrmPlayer, frmPlayer);
  Application.Run;
end.
