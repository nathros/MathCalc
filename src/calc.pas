unit Calc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, StdCtrls, ExtCtrls, StrUtils, Controls,
  Dialogs, Grids, Menus, Parser;

type

  { TCalcFrame }

  TCalcFrame = class(TFrame)
    InputButton1: TButton;
    InputButton0: TButton;
    InputButtonPower: TButton;
    InputButtonOpenBracket: TButton;
    InputButtonDivide: TButton;
    InputButtonCloseBracket: TButton;
    InputButtonEquals: TButton;
    InputButtonClear: TButton;
    InputButtonTimes: TButton;
    InputButtonMinus: TButton;
    InputButtonAdd: TButton;
    InputButtonPlusMinus: TButton;
    InputButtonDecimal: TButton;
    InputButton2: TButton;
    InputButton3: TButton;
    InputButton4: TButton;
    InputButton5: TButton;
    InputButton6: TButton;
    InputButton7: TButton;
    InputButton8: TButton;
    InputButton9: TButton;
    ExpressionInput: TEdit;
    HistoryMemo: TMemo;
    MenuAddVar: TMenuItem;
    MenuDelVar: TMenuItem;
    MenuChangeVar: TMenuItem;
    StoredVariablesPopupMenu: TPopupMenu;
    StoredVariables: TStringGrid;
    procedure ExpressionInputKeyPress(Sender: TObject; var Key: char);
    procedure InputButtonClick(Sender: TObject);
    procedure InputButtonClearClick(Sender: Tobject);
    procedure InputButtonEqualsClick(Sender: Tobject);
    procedure PlusMinusClick(Sender: TObject);
    procedure MenuAddVarClick(Sender: TObject);
    procedure MenuChangeVarClick(Sender: TObject);
    procedure MenuDelVarClick(Sender: TObject);
    procedure StoredVariablesContextPopup(Sender: TObject);
  private
    Parser: TParser;
  public
    procedure UpdateVarList;
  end; 

implementation

{ TCalcFrame }
{------------------------------------------------------------------------------}
procedure TCalcFrame.UpdateVarList;
var
  i, j: integer;
  List: TStringList;
begin
  try
    List:=Parser.Variable.GetAllVariables^;
    StoredVariables.Clean;
    StoredVariables.RowCount:=(List.Count div 2)+1;
    StoredVariables.Cells[0,0]:='Name';
    StoredVariables.Cells[1,0]:='Value';
    i:=0;
    j:=-1;
      while j<List.Count-1 do
        begin
          Inc(i);
          Inc(j);
          StoredVariables.Cells[0,i]:=List.Strings[j];
          Inc(j);
          StoredVariables.Cells[1,i]:=List.Strings[j];
        end;
  finally
    List.Free;
  end;
end;
{------------------------------------------------------------------------------}
procedure TCalcFrame.InputButtonEqualsClick(Sender: TObject);
begin
  if Parser=nil then Parser:=TParser.Create;
  HistoryMemo.Append(ExpressionInput.Text);
  ExpressionInput.Text:=FloatToStr(Parser.EvalInfix(ExpressionInput.Text));
  HistoryMemo.Append('Result '+ExpressionInput.Text);
end;
{------------------------------------------------------------------------------}
procedure TCalcFrame.StoredVariablesContextPopup(Sender: TObject);
begin
  if StoredVariables.Selection.Top<=0 then
    begin
      MenuChangeVar.Enabled:=false;
      MenuDelVar.Enabled:=false;
    end
  else
    begin
      MenuChangeVar.Enabled:=true;
      MenuDelVar.Enabled:=true;
    end;
  StoredVariablesPopupMenu.PopUp();
end;
{------------------------------------------------------------------------------}
procedure TCalcFrame.InputButtonClick(Sender: TObject);
begin
  ExpressionInput.Text:=ExpressionInput.Text+TButton(Sender).Caption;
end;
{------------------------------------------------------------------------------}
procedure TCalcFrame.PlusMinusClick(Sender: TObject);
var temp: string;
begin
  temp:=ExpressionInput.Text;
  if temp='' then Exit
  else if temp[1]='-' then temp:=MidStr(temp,2,length(temp))
  else
    begin
      temp:='-'+temp;
    end;
  ExpressionInput.Text:=temp;
end;
{------------------------------------------------------------------------------}
procedure TCalcFrame.InputButtonClearClick(Sender: TObject);
begin
  ExpressionInput.Clear;
end;
{------------------------------------------------------------------------------}
procedure TCalcFrame.MenuAddVarClick(Sender: TObject);
var
  VarName: string;
  VarResult: extended;
begin
  if Parser=nil then Parser:=TParser.Create;
  VarName:=InputBox('Input Name','Please input name.','');
  try
    if VarName='' then Exit
    else VarResult:=StrToFloat(InputBox('Input Value','Please input Value.',''));
  except
    Exit;
  end;
  Parser.Variable.AddVariable(VarName,VarResult);
  UpdateVarList;
end;
{------------------------------------------------------------------------------}
procedure TCalcFrame.MenuChangeVarClick(Sender: TObject);
var
  VarName: string;
  VarResult: extended;
begin
  VarName:=StoredVariables.Cells[0,StoredVariables.Selection.Top];
  try
    VarResult:=StrToFloat(InputBox('New Value',VarName,StoredVariables.Cells[1,StoredVariables.Selection.Top]));
  except
    Exit;
  end;
  Parser.Variable.ChangeVariable(VarName,VarResult);
  UpdateVarList;
end;
{------------------------------------------------------------------------------}
procedure TCalcFrame.MenuDelVarClick(Sender: TObject);
var VarName: string;
begin
  VarName:=StoredVariables.Cells[0,StoredVariables.Selection.Top];
  Parser.Variable.DelVariable(VarName);
  UpdateVarList;
end;
{------------------------------------------------------------------------------}
procedure TCalcFrame.ExpressionInputKeyPress(Sender: TObject; var Key: char);
begin
  if Ord(Key)=13 then InputButtonEqualsClick(Sender);
end;
{------------------------------------------------------------------------------}

initialization
  {$I calc.lrs}

end.
