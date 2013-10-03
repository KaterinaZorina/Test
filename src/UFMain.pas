unit UFMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtDlgs, Vcl.ExtCtrls, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    IOrigin: TImage;
    OPD: TOpenPictureDialog;
    BGetGreyscale: TButton;
    IResult: TImage;
    procedure FormActivate(Sender: TObject);
    procedure IOriginDblClick(Sender: TObject);
    procedure BGetGreyscaleClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

// ������� OnActivate ����������� � ������ ������ ��������� �����
procedure TForm1.BGetGreyscaleClick(Sender: TObject);
var
  Red, Green, Blue: array of array of byte;
  N, M: word; // ���������� ����� � �������� � �����������
  i, j: word;
  Color: TColor;
  r, g, b: byte; // ������������ ������������ � ������
begin
  N := IOrigin.Picture.Bitmap.Height;
  M := IOrigin.Picture.Bitmap.Width;
  SetLength(Red, N + 1); // +1 �.�. ��������� � 0
  SetLength(Green, N + 1); // +1 �.�. ��������� � 0
  SetLength(Blue, N + 1); // +1 �.�. ��������� � 0
  for i := 1 to N do
  begin
    SetLength(Red[i], M + 1);
    SetLength(Green[i], M + 1);
    SetLength(Blue[i], M + 1);
  end;
  for i := 1 to N do
    for j := 1 to M do
    begin
      Color := IOrigin.Canvas.Pixels[j - 1, i - 1];
      Red[i, j] := Color;
      Green[i, j] := Color shr 8;
      Blue[i, j] := Color shr 16;
    end;

  // TODO �������� �����������, ��� � ������� � ����������� ������� Red,Green,Blue. ���� ������ �������� ������ Y � ������������ ���

end;

procedure TForm1.FormActivate(Sender: TObject);
begin
  // IOrigin - �������� ���������� �� ������� ������. Canvas - ��� "����" ��� ���������
  IOrigin.Canvas.Pen.Color := clBlack;
  IOrigin.Canvas.Brush.Color := clWhite;
  IOrigin.Canvas.Brush.Style := bsSolid;
  // ������ �������������. ������ ��������� � ����� ������� ����. ������� ��������� ������, ����� �����
  IOrigin.Canvas.Rectangle(0, 0, IOrigin.Width - 1, IOrigin.Height - 1);

  // ���������� ��� ��������������� �����������
  IResult.Canvas.Pen.Color := clBlack;
  IResult.Canvas.Brush.Color := clWhite;
  IResult.Canvas.Brush.Style := bsSolid;
  IResult.Canvas.Rectangle(0, 0, IResult.Width - 1, IResult.Height - 1);
end;

// �������, ����������� ��� ������� ����� �� ������� ���������
procedure TForm1.IOriginDblClick(Sender: TObject);
begin
  // ��������� ������ �������� �������� �, ���� �������� ���� �������, ��������� �
  if OPD.Execute then
    IOrigin.Picture.LoadFromFile(OPD.FileName);
end;

end.
