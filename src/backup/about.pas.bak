unit About;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, ExtCtrls, StdCtrls,
  ParticleClasses;

type

  { TAboutForm }

  TAboutForm = class(TForm)
    Bevel1: TBevel;
    Bevel2: TBevel;
    btnClose: TButton;
    CircleMove: TTimer;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    procedure btnCloseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CircleMoveTimer(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    ParticleControl: TParticleControl;
  end; 

var
  AboutForm: TAboutForm;

implementation

{ TAboutForm }
{------------------------------------------------------------------------------}
procedure TAboutForm.btnCloseClick(Sender: TObject);
begin
  Close;
end;
{------------------------------------------------------------------------------}
procedure TAboutForm.FormCreate(Sender: TObject);
var i: byte;
begin
  ParticleControl:=TParticleControl.Create(Bevel2.Parent);
  ParticleControl.SetBounds(Bevel2.Left+1,Bevel2.Top+1,Bevel2.Width-2,Bevel2.Height-2);
  for i:=1 to 3 do
    begin
      ParticleControl.AddParticle;
      TParticle(FindComponent('Circle'+IntToStr(i))).SendToBack;
    end;
end;
{------------------------------------------------------------------------------}
procedure TAboutForm.CircleMoveTimer(Sender: TObject);
begin
  ParticleControl.MoveParticles;
end;
{------------------------------------------------------------------------------}
procedure TAboutForm.FormHide(Sender: TObject);
begin
  CircleMove.Enabled:=false;
end;
{------------------------------------------------------------------------------}
procedure TAboutForm.FormShow(Sender: TObject);
begin
  CircleMove.Enabled:=true;
end;
{------------------------------------------------------------------------------}

initialization
  {$I about.lrs}
  Randomize;

end.
