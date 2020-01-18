unit MathTopic;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, ComCtrls, ExtCtrls, Buttons, StdCtrls,
  Arrow, Controls;

type
  TPageData = record
    Stage: shortint;
    Difficulty: byte;
    X: integer;
    Y: integer;
    XCo1: integer; // Co for Coefficient.
    YCo1: integer;
    XCo2: integer;
    YCo2: integer;
    Res1: integer;
    Res2: integer;
  end;

type

  { TMathTopicFrame }

  TMathTopicFrame = class(TFrame)
    Blank: TPage;
    BlankLabel: TLabel;
    SelDif: TComboBox;
    SimEquLabelExp1: TLabel;
    SimEquLine: TShape;
    SimEquLabel3: TLabel;
    SimEquLabel4: TLabel;
    SimEquAdd: TLabel;
    SimEquNext: TButton;
    SimEquCombo: TComboBox;
    ListBox1: TListBox;
    SimEquLabel1: TLabel;
    SimEquLabel2: TLabel;
    Item1: TPage;
    TriandImp: TPage;
    ShowHideTree: TArrow;
    Button3: TButton;
    Notebook: TNotebook;
    SimEqu: TPage;
    TopicTree: TTreeView;
    procedure ShowHideTreeClick(Sender: TObject);
    procedure SimEquNextClick(Sender: TObject);
    procedure TopicTreeSelectionChanged(Sender: TObject);
  private
    SimEquData: TPageData;
  public
    procedure GenVariables(const PageName: string);
    procedure GenSimEqu(const GenStage: byte);
  end; 

implementation

{------------------------------------------------------------------------------}
function CheckMinus(const Data: integer): string;
begin
  if Data<0 then Result:='x'
  else Result:='x+';
end;
{------------------------------------------------------------------------------}

{ TMathTopicFrame }
{------------------------------------------------------------------------------}
procedure TMathTopicFrame.TopicTreeSelectionChanged(Sender: TObject);
var
  temp, NoSpaces: string;
  i: byte;
begin
  temp:=TopicTree.Selected.Text;
  NoSpaces:='';
  NoSpaces:=temp[1]+temp[2]+temp[3];
  for i:=4 to Length(temp) do
    if temp[i]=' ' then NoSpaces:=NoSpaces+temp[i+1]+temp[i+2]+temp[i+3];
  //Notebook.ActivePage:=NoSpaces;
  GenVariables(NoSpaces);
end;
{------------------------------------------------------------------------------}
procedure TMathTopicFrame.ShowHideTreeClick(Sender: TObject);
begin
  case TopicTree.Visible of
    true: begin
      ShowHideTree.Left:=8;
      Notebook.Left:=32;
      ShowHideTree.ArrowType:=atRight;
    end;
    false: begin
      ShowHideTree.Left:=TopicTree.Width+8;
      Notebook.Left:=TopicTree.Width+ShowHideTree.Width+8;
      ShowHideTree.ArrowType:=atLeft;
    end;
  end;
  TopicTree.Visible:=not TopicTree.Visible;
  ShowHideTree.Refresh;
end;
{------------------------------------------------------------------------------}
procedure TMathTopicFrame.SimEquNextClick(Sender: TObject);
begin
  GenSimEqu(SimEquData.Stage);
end;
{------------------------------------------------------------------------------}
procedure TMathTopicFrame.GenVariables(const PageName: string);
begin
  {case Notebook.IndexOf(TPage(FindComponent(PageName))) of
    0: GenSimEqu(1);
  end; }
end;
{------------------------------------------------------------------------------}
procedure TMathTopicFrame.GenSimEqu(const GenStage: byte);
const br = sLineBreak;
var
  temp1, temp2: integer;
  space: string = '';
  tempStr: string = '';
begin
  case GenStage of
    1:begin
        with SimEquData do
          begin
            SelDif.Visible:=true;
            SimEquLine.Visible:=false;
            SimEquCombo.Visible:=true;
            SimEquNext.Visible:=true;
            SimEquLabelExp1.Caption:='Here there are two equations.';
            SimEquLabelExp1.Visible:=true;
            X:=Random(21)-10;
            Y:=Random(21)-10;
            XCo1:=Random(21)-10;
            YCo1:=Random(21)-10;
            XCo2:=Random(21)-10;
            YCo2:=Random(21)-10;
            Res1:=(XCo1*X)+(YCo1*Y);
            Res2:=(XCo2*X)+(YCo2*Y);
            space:=CheckMinus(YCo1);
            SimEquLabel1.Caption:=IntToStr(XCo1)+space+IntToStr(YCo1)+'y='+IntToStr(Res1);
            space:=CheckMinus(YCo2);
            SimEquLabel2.Caption:=IntToStr(XCo2)+space+IntToStr(YCo2)+'y='+IntToStr(Res2);
          end;
      end;
    2:begin
        SelDif.Visible:=false;
        case SimEquCombo.ItemIndex of
          0:begin
              tempStr:='(1) '+SimEquLabel1.Caption+'  *'+IntToStr(SimEquData.XCo2);
              space:=  '(2) '+SimEquLabel2.Caption+'  *'+IntToStr(SimEquData.XCo1);
            end;
          1:begin
              tempStr:='(1) '+SimEquLabel1.Caption+'  *'+IntToStr(SimEquData.YCo2);
              space:=  '(2) '+SimEquLabel2.Caption+'  *'+IntToStr(SimEquData.YCo1);
            end;
        end;
        SimEquCombo.Visible:=false;
        SimEquLabel1.Caption:=tempStr;
        SimEquLabel2.Caption:=space;
      end;
    3:begin
        with SimEquData do
          begin
            if SimEquCombo.ItemIndex=0 then
              begin
                temp1:=XCo1;
                temp2:=XCo2;
              end
            else
              begin
                temp1:=YCo1;
                temp2:=YCo2;
              end;
            XCo1:=XCo1*temp2;
            YCo1:=YCo1*temp2;
            XCo2:=XCo2*temp1;
            YCo2:=YCo2*temp1;
            Res1:=(X*XCo1)+(Y*YCo1);
            Res2:=(X*XCo2)+(Y*YCo2);
            space:=CheckMinus(YCo1);
            SimEquLabel1.Caption:=IntToStr(XCo1)+space+IntToStr(YCo1)+'y='+IntToStr(Res1);
            space:=CheckMinus(YCo2);
            SimEquLabel2.Caption:=IntToStr(XCo2)+space+IntToStr(YCo2)+'y='+IntToStr(Res2);
            case SimEquCombo.ItemIndex of
              0:begin
                  if (XCo1>0) and (XCo2<0) or (XCo1<0) and (XCo2>0) then SimEquAdd.Caption:='+'
                  else SimEquAdd.Caption:='-';
                end;
              1:begin
                  if (YCo1>0) and (YCo2<0) or (YCo1<0) and (YCo2>0) then SimEquAdd.Caption:='+'
                  else SimEquAdd.Caption:='-';
                end;
            end;
            SimEquLine.Visible:=true;
            SimEquAdd.Visible:=true;
          end;
      end;
    4:begin
        with SimEquData do
          begin
            case SimEquCombo.ItemIndex of
              0:begin
                  space:=IntToStr(YCo1-YCo2)+'y='+IntToStr(Res1-Res2)+br;
                  space:=space+'   y='+IntToStr((Res1-Res2) div (YCo1-YCo2));
                end;
              1:begin
                  space:=IntToStr(XCo1-XCo2)+'x='+IntToStr(Res1-Res2)+br;
                  space:=space+'   x='+IntToStr((Res1-Res2) div (XCo1-XCo2));
                end;
            end;
            SimEquLabel3.Caption:=space;
          end;
      end;
    5:begin
        with SimEquData do
          begin
            case SimEquCombo.ItemIndex of
            0:begin
                space:=CheckMinus(YCo1);
                space:=IntToStr(XCo1)+space+IntToStr(yCo1)+'('+IntToStr(y)+')='+IntToStr(Res1)
                +br+inttostr(XCo1)+'x='+inttostr(Res1-(yCo1*y))
                +br+'x='+inttostr((Res1-(yCo1*y)) div XCo1);
              end;
            1:begin
                if YCo1<0 then space:='-' else space:='+';
                space:=IntToStr(XCo1)+'('+IntToStr(x)+')'+space+IntToStr(YCo1)+'y='+IntToStr(Res1)
                +br+IntToStr(YCo1)+'y='+IntToStr(Res1-(XCo1*x))
                +br+'y='+IntToStr((Res1-(XCo1*x)) div YCo1);
              end;
            end;
            SimEquLabel4.Caption:=space;
            SimEquNext.Visible:=false;
            Stage:=-1;
          end;
      end;
  end;

  //debug info begin
  {with simequdata do
  begin

      Listbox1.Items.Add('x '+Inttostr(X));
      Listbox1.Items.Add('y '+Inttostr(y));
      Listbox1.Items.Add('x1 '+Inttostr(XCo1));
      Listbox1.Items.Add('y1 '+Inttostr(yCo1));
      Listbox1.Items.Add('x2 '+Inttostr(XCo2));
      Listbox1.Items.Add('y2 '+Inttostr(yCo2));

      Listbox1.Items.Add('stage '+Inttostr(simequdata.Stage));
  end;     }
  //debug info end
   SimEquData.Stage:=GenStage+1;

end;
{------------------------------------------------------------------------------}

initialization
  {$I mathtopic.lrs}
  Randomize;

end.
