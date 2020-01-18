unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Dialogs, Menus, ComCtrls,
  ExtCtrls, Calc, About, NumberGen, UnitConvert, MathTopic, Graph;

type

  { TMainForm }

  TMainForm = class(TForm)
    CalcFrame: TCalcFrame;
    MainMenu: TMainMenu;
    MathTopicFrame: TMathTopicFrame;
    MenuGraph: TMenuItem;
    MenuRevise: TMenuItem;
    MenuNumGen: TMenuItem;
    MenuAbout: TMenuItem;
    MenuHelp: TMenuItem;
    MenuTools: TMenuItem;
    MenuUnitConv: TMenuItem;
    StatusBar: TStatusBar;
    procedure MenuAboutClick(Sender: TObject);
    procedure MenuGraphClick(Sender: TObject);
    procedure MenuNumGenClick(Sender: TObject);
    procedure MenuReviseClick(Sender: TObject);
    procedure MenuUnitConvClick(Sender: TObject);

  private
    { private declarations }
  public
    { public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{ TMainForm }
{------------------------------------------------------------------------------}
procedure TMainForm.MenuUnitConvClick(Sender: TObject);
begin
  UnitConvertForm.Show;
end;
{------------------------------------------------------------------------------}
procedure TMainForm.MenuAboutClick(Sender: TObject);
begin
  AboutForm.ShowModal;
end;
{------------------------------------------------------------------------------}
procedure TMainForm.MenuGraphClick(Sender: TObject);
begin
  GraphForm.Show;
end;
{------------------------------------------------------------------------------}
procedure TMainForm.MenuNumGenClick(Sender: TObject);
begin
  NumberGenForm.Show;
end;
{------------------------------------------------------------------------------}
procedure TMainForm.MenuReviseClick(Sender: TObject);
begin
  if CalcFrame.Visible then
    begin
      MainForm.Width:=MathTopicFrame.Width+8;
      MainForm.Height:=MathTopicFrame.Height+StatusBar.Height+24;
      MenuRevise.Caption:='Calculator';
    end
  else
    begin
      MainForm.Width:=CalcFrame.Width+8;
      MainForm.Height:=CalcFrame.Height+StatusBar.Height+32;
      MenuRevise.Caption:='Revise Topic';
    end;
  CalcFrame.Visible:=not CalcFrame.Visible;
  MathTopicFrame.Visible:=not MathTopicFrame.Visible;
end;
{------------------------------------------------------------------------------}

initialization
  {$I main.lrs}

end.
