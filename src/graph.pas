unit Graph;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Parser;

type

  { TGraphForm }

  TGraphForm = class(TForm)
    FunctionInput: TEdit;
    PaintArea: TPaintBox;
    procedure FunctionInputKeyPress(Sender: TObject; var Key: char);
    procedure FormCreate(Sender: TObject);
    procedure PaintAreaPaint(Sender: TObject);
  private
    Parser: TParser;
    XRange: integer;
    YRange: integer;
    XRangePixels: integer;
    YRangePixels: integer;
    CorrectX, CorrectY: integer;
    Origin: TPoint;
  public
    x: extended;
    procedure DrawAxis;
    procedure DrawGraph;
  end; 

var
  GraphForm: TGraphForm;

implementation

{ TGraphForm }
{------------------------------------------------------------------------------}
procedure TGraphForm.FunctionInputKeyPress(Sender: TObject; var Key: char);
begin
  if Ord(Key)=13 then
  if FunctionInput.Text<>'' then PaintArea.Refresh;
end;
{------------------------------------------------------------------------------}
procedure TGraphForm.FormCreate(Sender: TObject);
begin
  Parser:=TParser.Create;
  Parser.Variable.AddVariable('x',0);
  Origin.x:=PaintArea.Width div 2;
  Origin.y:=PaintArea.Height div 2;
end;
{------------------------------------------------------------------------------}
procedure TGraphForm.PaintAreaPaint(Sender: TObject);
begin
  if FunctionInput.Text='' then DrawAxis
  else DrawGraph;
end;
{------------------------------------------------------------------------------}
procedure TGraphForm.DrawAxis;
begin
  PaintArea.Canvas.MoveTo(Origin.x+9,0);
  PaintArea.Canvas.LineTo(Origin.x+9,PaintArea.Height);
  PaintArea.Canvas.MoveTo(PaintArea.Width,Origin.y+16);
  PaintArea.Canvas.LineTo(0,Origin.y+16);
end;
{------------------------------------------------------------------------------}
procedure TGraphForm.DrawGraph;
var
  Points: array[0..41] of TPoint;
  i, j: integer;
  res: extended;
begin
  DrawAxis;
  Origin:=Point(PaintArea.Width div 2,PaintArea.Height div 2);
  XRange:=20;
  YRange:=20;
  XRangePixels:=PaintArea.Width div (XRange*2);
  YRangePixels:=PaintArea.Height div (YRange*2);
  correctx:=XRangePixels mod XRange;
  correcty:=YRangePixels mod YRange;
  j:=0;
  try
    for i:=(XRange*-1) to XRange do
      begin
        Parser.Variable.ChangeVariable('x',i);
        res:=Parser.EvalInfix(FunctionInput.Text);
        Inc(j);
        Points[j].X:=((i+XRange)*XRangePixels)+CorrectX;
        Points[j].Y:=PaintArea.Height-((Trunc(res)+YRange)*YRangePixels)+CorrectY;
      end;
    PaintArea.Canvas.Polyline(Points);
  except
  end;
end;
{------------------------------------------------------------------------------}

initialization
  {$I graph.lrs}

end.
