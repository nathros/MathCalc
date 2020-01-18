unit UnitConvert;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, StdCtrls, ConvUtils, StdConvs;

type

  { TUnitConvertForm }

  TUnitConvertForm = class(TForm)
    ConvertValue: TEdit;
    ResultValue: TEdit;
    Filter: TComboBox;
    ResultList: TListBox;
    ConvertList: TListBox;
    procedure ConvertListSelectionChange(Sender: TObject; User: boolean);
    procedure ConvertValueChange(Sender: TObject);
    procedure ConvertKeyPress(Sender: TObject; var Key: char);
    procedure FilterChangeBounds(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ResultListSelectionChange(Sender: TObject; User: boolean);
    procedure ResultValueChange(Sender: TObject);
  private
    { private declarations }
  public
    procedure ReturnConvTypes(var ConvertTo, ConvertFrom: TConvType);
  end;

var
  UnitConvertForm: TUnitConvertForm;

implementation

{ TUnitConvertForm }
{------------------------------------------------------------------------------}
procedure TUnitConvertForm.FormCreate(Sender: TObject);
var
  LFamily: TConvFamilyArray;
  FilterList: TStringList;
  i: byte;
begin
  GetConvFamilies(LFamily); // Get ConvertList of unit families.
  FilterList:=TStringList.Create;
  try
    for i:=0 to Length(LFamily)-1 do
      FilterList.Add(ConvFamilyToDescription(i));
    Filter.Items:=FilterList;
    Filter.ItemIndex:=0;
  finally
    FilterList.Free;
  end;
  FilterChangeBounds(Sender);
end;
{------------------------------------------------------------------------------}
procedure TUnitConvertForm.ResultListSelectionChange(Sender: TObject;
  User: boolean);
begin
  ResultValue.OnChange:=nil;
  ConvertValueChange(Sender);
  ResultValue.OnChange:=@ResultValueChange;
end;
{------------------------------------------------------------------------------}
procedure TUnitConvertForm.ResultValueChange(Sender: TObject);
var ConvertTo, ConvertFrom: TConvType;
begin
  if ConvertValue.Focused=true then Exit;
  ReturnConvTypes(ConvertTo,ConvertFrom);
  try
    ConvertValue.Text:=FloatToStr(Convert(StrToFloat(ResultValue.Text),ConvertTo,ConvertFrom));
  except
    ConvertValue.Text:='';
  end;
end;
{------------------------------------------------------------------------------}
procedure TUnitConvertForm.FilterChangeBounds(Sender: TObject);
var
  LFamily: TConvFamily;
  LTypes: TConvTypeArray;
  i: byte;
begin
  LFamily:=Filter.ItemIndex;
  GetConvTypes(LFamily, LTypes);
  ConvertList.Clear;
  for i:=0 to Length(LTypes)-1 do
    ConvertList.Items.Add(ConvTypeToDescription(LTypes[i]));
  ConvertList.ItemIndex:=0;
  ResultList.ItemIndex:=0;
end;
{------------------------------------------------------------------------------}
procedure TUnitConvertForm.ConvertListSelectionChange(Sender: TObject;
  User: boolean);
var
  LFamily: TConvFamily;
  LTypes: TConvTypeArray;
  CurrentType: TConvType;
  i: byte;
begin
  CurrentType:=TListBox(Sender).ItemIndex;
  LFamily:=Filter.ItemIndex;
  GetConvTypes(LFamily, LTypes);

  ResultList.Clear;
  for i:=0 to Length(LTypes)-1 do
    if i<>CurrentType then ResultList.Items.Add(ConvTypeToDescription(LTypes[i]));
  ResultList.ItemIndex:=0;
  ConvertValueChange(Sender);
end;
{------------------------------------------------------------------------------}
procedure TUnitConvertForm.ConvertValueChange(Sender: TObject);
var ConvertTo, ConvertFrom: TConvType;
begin
  if ResultValue.Focused=true then Exit;
  ReturnConvTypes(ConvertTo,ConvertFrom);
  try
    ResultValue.Text:=FloatToStr(Convert(StrToFloat(ConvertValue.Text),ConvertFrom,ConvertTo));
  except
    ResultValue.Text:='';
  end;
end;
{------------------------------------------------------------------------------}
procedure TUnitConvertForm.ConvertKeyPress(Sender: TObject; var Key: char);
var i: integer;
begin
  if not(Key in['0'..'9','.',Chr(8)]) then Key:=Chr(0)
  else if Key='.' then
    for i:=1 to Length(TEdit(Sender).Text) do
      if TEdit(Sender).Text[i]='.' then
        begin
          Key:=Chr(0);
          Break;
        end;
end;
{------------------------------------------------------------------------------}
procedure TUnitConvertForm.ReturnConvTypes(var ConvertTo, ConvertFrom: TConvType);
var
  LFamily: TConvFamily;
  LTypes:  TConvTypeArray;
begin
  LFamily:=Filter.ItemIndex;
  ConvertFrom:=ConvertList.ItemIndex;
  ConvertTo:=ResultList.ItemIndex;
  GetConvTypes(LFamily, LTypes);
  ConvertFrom:=ConvertFrom+LTypes[0];
  ConvertTo:=ConvertTo+LTypes[0];
  if ConvertFrom<=ConvertTo then ConvertTo:=ConvertTo+1;
end;
{------------------------------------------------------------------------------}

initialization
  {$I unitconvert.lrs}

end.
