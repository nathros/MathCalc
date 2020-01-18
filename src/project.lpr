program project;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, Main, LResources, UnitConvert, UnitEdit,
  About, NumberGen, Graph, Calc, MathTopic;

{$IFDEF WINDOWS}{$R project.rc}{$ENDIF}

begin
  {$I project.lrs}
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TUnitConvertForm, UnitConvertForm);
  Application.CreateForm(TNumberGenForm, NumberGenForm); // Find out about threads
  //Application.CreateForm(TUnitEditForm, UnitEditForm); // Abandoned
  Application.CreateForm(TAboutForm, AboutForm);
  Application.CreateForm(TGraphForm, GraphForm); // Needs work.
  Application.Run;
end.

