unit ParticleClasses; // Just for fun

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, Controls;

type

  { TParticle }

  TParticle = class(TShape)
  private
    NextParticle: TParticle;
    procedure ReverseLeft;
    procedure ReverseTop;
  public
    LeftMove: integer;
    TopMove: integer;
	constructor Create(TheOwner: TComponent); override;
  end;

type

  { TParticleControl }

  TParticleControl = class
  private
    FirstParticle: TParticle;
    LastParticle: TParticle;
    ObjectParent: TWinControl;
    NumOfParticles: integer;
    StartX: integer;
    StartY: integer;
    WidthX: integer;
    HeightY: integer;
    procedure Collision(Particle: TParticle);
    procedure SwapForces(First, Second: TParticle);
  public
    constructor Create(Parent: TWinControl);
    destructor Free;
    procedure SetBounds(X, Y, X1, Y1: integer);
    procedure MoveParticles;
    procedure AddParticle;
    procedure DelParticle;
  end;

implementation

{TParticle}

constructor TParticle.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  LeftMove:=Random(20)-10;
  while LeftMove=0 do LeftMove:=Random(20)-10;
  TopMove:=Random(20)-10;
  while TopMove=0 do TopMove:=Random(20)-10;
  NextParticle:=nil;
end;

procedure TParticle.ReverseLeft;
begin
  LeftMove:=leftMove*-1;
end;

procedure TParticle.ReverseTop;
begin
  TopMove:=TopMove*-1;
end;

{TParticleControl}

constructor TParticleControl.Create(Parent: TWinControl);
begin
  inherited Create;
  NumOfParticles:=0;
  ObjectParent:=Parent;
  LastParticle:=nil;
  FirstParticle:=nil;
end;

destructor TParticleControl.Free;
begin
  while FirstParticle<>nil do DelParticle;
end;

procedure TParticleControl.Collision(Particle: TParticle);
var Compare: TParticle;
begin
  if Particle.NextParticle<>nil then Collision(Particle.NextParticle);
  Compare:=Particle.NextParticle;
  while Compare<>nil do
    begin
      if (((Particle.Left>=Compare.Left) and (Particle.Left<=Compare.Left+32)) or
          ((Particle.Left<=Compare.Left) and (Particle.Left+32>=Compare.Left))) and
         (((Particle.Top>=Compare.Top) and (Particle.Top<=Compare.Top+32)) or
          ((Particle.Top<=Compare.Top) and (Particle.Top+32>=Compare.Top))) then
        begin
          SwapForces(Particle,Compare);
        end;
      Compare:=Compare.NextParticle;
    end;
end;

procedure TParticleControl.SwapForces(First, Second: TParticle);
var store: integer;
begin
  Store:=First.LeftMove;
  First.LeftMove:=Second.leftMove;
  Second.leftMove:=Store;

  Store:=First.TopMove;
  First.TopMove:=Second.TopMove;
  Second.TopMove:=Store;

  First.Left:=First.Left+(First.LeftMove div 2);     //  Sometimes Particles become
  First.Top:=First.Top+(First.TopMove div 2);        //  stuck, this prevents it by
  Second.Left:=Second.Left+(Second.LeftMove div 2);  //  moving them acording to
  Second.Top:=Second.Top+(Second.TopMove div 2);     //  their new forces.
end;

procedure TParticleControl.SetBounds(X, Y, X1, Y1: integer);
begin
  StartX:=X;
  StartY:=Y;
  WidthX:=X1;
  HeightY:=Y1;
end;

procedure TParticleControl.MoveParticles;
var Particle: TParticle;
begin
  if NumOfParticles=0 then Exit;
  Particle:=FirstParticle;
  while Particle<>nil do
    begin
      Particle.Left:=Particle.Left+Particle.LeftMove;
      Particle.Top:=Particle.Top+Particle.TopMove;
      // Border collision
      if Particle.Left<=StartX then
        begin
          Particle.ReverseLeft;
          Particle.Left:=StartX;
        end
      else if Particle.Left>StartX+WidthX-32 then
        begin
          Particle.ReverseLeft;
          Particle.Left:=StartX+WidthX-32;
        end;
      if Particle.Top<=StartY then
        begin
          Particle.ReverseTop;
          Particle.Top:=StartY;
        end
      else if Particle.Top>=StartY+HeightY-32 then
        begin
          Particle.ReverseTop;
          Particle.Top:=StartY+HeightY-32;
        end;
      // End border collision
      Particle:=Particle.NextParticle;
    end;
  Collision(FirstParticle);
end;

procedure TParticleControl.AddParticle;
var New: TParticle;
begin
  Inc(NumOfParticles);
  New:=TParticle.Create(ObjectParent);
  with New do
    begin
      Width:=32;
      Height:=32;
      Left:=Random(WidthX-Width)+StartX;
      Top:=Random(HeightY-Height)+StartY;
      Shape:=stCircle;
      Name:='Circle'+IntToStr(NumOfParticles);
      Parent:=ObjectParent;
    end;
  if NumOfParticles=1 then FirstParticle:=New
  else if NumOfParticles=2 then
    begin
      FirstParticle.NextParticle:=New;
      LastParticle:=New;
    end
  else
    begin
      LastParticle.NextParticle:=new;
      LastParticle:=New;
    end;
end;

procedure TParticleControl.DelParticle;
var
  Current: TParticle;
  i: integer;
begin
  if NumOfParticles=0 then Exit;
  Dec(NumOfParticles);
  if NumOfParticles=0 then
    begin
      FirstParticle.Free;
      FirstParticle:=nil;
      LastParticle:=nil;
    end
  else
    begin
      Current:=FirstParticle;
      for i:=1 to NumOfParticles-1 do Current:=Current.NextParticle;
      LastParticle:=Current;
      Current:=Current.NextParticle;
      Current.Free;
      LastParticle.NextParticle:=nil;
    end;
end;

end.

