program box80_v00;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, fbox80, uprocessor, usio, uterminal, fterminal, fabout, uglobals, 
  ucflash, uconfigdefs, uvirtual, uxml, fcfinfo, ucircular, fmemory;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TfrmBox80, frmBox80);
  Application.CreateForm(TfrmTerminal, frmTerminal);
  Application.CreateForm(TfrmHelpAbout, frmHelpAbout);
  Application.CreateForm(TfrmCFinfo, frmCFinfo);
  Application.CreateForm(TfrmMemory, frmMemory);
  Application.Run;
end.

