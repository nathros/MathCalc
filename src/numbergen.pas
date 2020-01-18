unit NumberGen;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, StdCtrls, Grids,
  ExtCtrls, ComCtrls, Spin;

type

  { TNumberGenForm }

  TNumberGenForm = class(TForm)
    GenSelect: TComboBox;
    LabelStart: TLabel;
    LabelEnd: TLabel;
    MethodGroup: TGroupBox;
    Method1Radio: TRadioButton;
    Method2Radio: TRadioButton;
    MethodEnd: TSpinEdit;
    MethodStart: TSpinEdit;
    OutputList: TListBox;
    Start: TButton;
    procedure Method1RadioClick(Sender: TObject);
    procedure Method2RadioClick(Sender: TObject);
    procedure StartClick(Sender: TObject);
  private
    { private declarations }
  public
    procedure GenPrime;
    procedure GenSquare;
    procedure GenTriangle;
    procedure GenComposite;

    function IsPrime(const Test: integer): boolean;
    procedure GetMethod(out Param1, Param2: integer; out Method: boolean);
  end; 

var
  NumberGenForm: TNumberGenForm;

implementation

{ TNumberGenForm }
{------------------------------------------------------------------------------}
procedure TNumberGenForm.StartClick(Sender: TObject);
begin
  OutputList.Clear;
  case GenSelect.ItemIndex of
    0: GenPrime;
    1: GenSquare;
    2: GenTriangle;
    3: GenComposite;
  end;
end;
{------------------------------------------------------------------------------}
procedure TNumberGenForm.Method1RadioClick(Sender: TObject);
begin
  LabelStart.Caption:='Starting number';
  labelEnd.Caption:='Numbers to find';
end;
{------------------------------------------------------------------------------}
procedure TNumberGenForm.Method2RadioClick(Sender: TObject);
begin
  LabelStart.Caption:='Start of range';
  labelEnd.Caption:='End of range';
end;
{------------------------------------------------------------------------------}
procedure TNumberGenForm.GetMethod(out Param1, Param2: integer; out Method: boolean);
begin
  Method:=Method1Radio.Checked;
  Param1:=StrToInt(MethodStart.Text);
  Param2:=StrToInt(MethodEnd.Text);
end;
{------------------------------------------------------------------------------}
function TNumberGenForm.IsPrime(const Test: integer): boolean;
var i, max: integer;
begin
  if (Test<=1) or ((Test mod 2)=0) and (Test<>2) then
    begin
      Result:=false;
      Exit;
    end;
  max:=Trunc(Sqrt(Test));
  i:=1;
  while i<max do
    begin
      Inc(i,2);
      if (Test mod i)=0 then
        begin
          Result:=false;
          Exit;
        end;
    end;
  Result:=true;
end;
{------------------------------------------------------------------------------}
procedure TNumberGenForm.GenPrime;
var
  StartValue, EndValue, count: integer;
  method: boolean;
begin
  GetMethod(StartValue,EndValue,Method);
  if Method then
    begin
      count:=0;
      while count<EndValue do
        begin
          if IsPrime(StartValue) then
            begin
              Inc(count);
              OutputList.Items.Add(IntToStr(StartValue));
            end;
          Inc(StartValue);
        end;
    end
  else
    while StartValue<=EndValue do
      begin
        if IsPrime(StartValue) then OutputList.Items.Add(IntToStr(StartValue));
        Inc(StartValue);
      end;
end;
{------------------------------------------------------------------------------}
procedure TNumberGenForm.GenSquare;
var
  StartValue, EndValue: integer;
  Method: boolean;
begin
  GetMethod(StartValue,EndValue,Method);
  if StartValue=0 then StartValue:=1;
  for StartValue:=StartValue to EndValue do
    case Method of
      true: OutputList.Items.Add(IntToStr(StartValue*StartValue));
      false: if Frac(Sqrt(StartValue))=0 then OutputList.Items.Add(IntToStr(StartValue));
    end;
end;
{------------------------------------------------------------------------------}
procedure TNumberGenForm.GenTriangle;
var
  StartValue, EndValue, count, nxt, total: integer;
  Method: boolean;
begin
  GetMethod(StartValue,EndValue,Method);
  count:=0; total:=0; nxt:=0;
  if Method then
    while count<EndValue do
      begin
        Inc(nxt);
        total:=total+nxt;
        if total>=StartValue then
          begin
            OutputList.Items.Add(IntToStr(total));
            Inc(count);
          end;
      end
  else
    while total<EndValue do
      begin
        Inc(nxt);
        total:=total+nxt;
        if (total>=StartValue) and (total<=EndValue) then OutputList.Items.Add(IntToStr(total));
      end;
end;
{------------------------------------------------------------------------------}
procedure TNumberGenForm.GenComposite;
var
  StartValue, EndValue, count: integer;
  method: boolean;
begin
  GetMethod(StartValue,EndValue,Method);
  if StartValue<=2 then StartValue:=3;
  if Method then
    begin
      count:=0;
      while count<EndValue do
        begin
          if not(IsPrime(StartValue)) then
            begin
              Inc(count);
              OutputList.Items.Add(IntToStr(StartValue));
            end;
          Inc(StartValue);
        end;
    end
  else
    while StartValue<=EndValue do
      begin
        if not(IsPrime(StartValue)) then OutputList.Items.Add(IntToStr(StartValue));
        Inc(StartValue);
      end;
end;
{------------------------------------------------------------------------------}

initialization
  {$I numbergen.lrs}

end.
