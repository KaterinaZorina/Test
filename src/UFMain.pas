unit UFMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.ExtDlgs, Vcl.ExtCtrls, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    IOrigin: TImage;
    OPD: TOpenPictureDialog;
    BGetGreyscale: TButton;
    IResult: TImage;
    Ethresold: TEdit;
    procedure FormActivate(Sender: TObject);
    procedure IOriginDblClick(Sender: TObject);
    procedure BGetGreyscaleClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  BI: array of array of boolean;
  MarkedImg: array of array of word;
  Thresold: byte;
  N, M: word;

implementation

{$R *.dfm}

procedure RecursiveMark(i, j, Mark: word);
begin
  if (i in [1 .. N]) and (j in [1 .. M]) and (MarkedImg[i, j] = 1) then
  begin
    MarkedImg[i, j] := Mark;
    RecursiveMark(i - 1, j, Mark);
    RecursiveMark(i + 1, j, Mark);
    RecursiveMark(i, j - 1, Mark);
    RecursiveMark(i, j + 1, Mark);
  end;
end;

// Событие OnActivate выполняется в момент первой активации формы
procedure TForm1.BGetGreyscaleClick(Sender: TObject);
var
  Y: real;
  // N, M: word; // количество строк и столбцов в изображении
  i, j: word;
  Color: TColor;
  r, g, b, l: byte; // интенсивости составляющих в пиксле
  BM: TBitMap;
  MarkInd: word;
begin
  N := IOrigin.Picture.Bitmap.Height;
  M := IOrigin.Picture.Bitmap.Width;
  SetLength(BI, N + 1);
  SetLength(MarkedImg, N + 1);
  for i := 1 to N do
  begin
    SetLength(BI[i], M + 1);
    SetLength(MarkedImg[i], M + 1);
  end;

  for i := 1 to N do
    for j := 1 to M do
    begin
      Color := IOrigin.Canvas.Pixels[j - 1, i - 1];
      r := Color;
      g := Color shr 8;
      b := Color shr 16;
      Y := 0.299 * r + 0.587 * g + 0.114 * b;
      Thresold := StrToInt(Ethresold.Text);
      BI[i, j] := Y > Thresold;
    end;

  BM := TBitMap.Create;
  BM.Height := N;
  BM.Width := M;
  for i := 0 to N - 1 do
    for j := 0 to M - 1 do
      if BI[i + 1, j + 1] then
        BM.Canvas.Pixels[j, i] := clWhite
      else
        BM.Canvas.Pixels[j, i] := clBlack;
  IResult.Picture.Assign(BM);
  BM.Free;

  // Изначально у нас объекты переднего плана кодируются 1, фон - 0
  for i := 1 to N do
    for j := 1 to M do
      MarkedImg[i, j] := byte(not BI[i, j]);

  MarkInd := 1;
  for i := 1 to N do
    for j := 1 to M do
      // Ищем пикселы со значением 1 (т.е. первую точку объекта)
      if MarkedImg[i, j] = 1 then
      begin
        // Выбираем следующую метку
        MarkInd := MarkInd + 1;
        // Начиная с одной точки заливаем весь объект
        RecursiveMark(i, j, MarkInd);
      end;

  BM := TBitMap.Create;
  BM.Height := N;
  BM.Width := M;
  for i := 0 to N - 1 do
    for j := 0 to M - 1 do
      // В зависимоти от метки назначаем цвет, как ты понимаешь, тут только два цвета
      case MarkedImg[i + 1, j + 1] of
        2:
          BM.Canvas.Pixels[j, i] := clGreen;
        3:
          BM.Canvas.Pixels[j, i] := clRed;
      else
        BM.Canvas.Pixels[j, i] := clWhite;
      end;
  IResult.Picture.Assign(BM);
  BM.Free;
end;

procedure TForm1.FormActivate(Sender: TObject);
begin
  // IOrigin - название компонента на котором рисуем. Canvas - его "поле" для рисования
  IOrigin.Canvas.Pen.Color := clBlack;
  IOrigin.Canvas.Brush.Color := clWhite;
  IOrigin.Canvas.Brush.Style := bsSolid;
  // Рисуем прямоугольник. Начало координат в левом верхнем углу. Сначала указываем ширину, потом длину
  IOrigin.Canvas.Rectangle(0, 0, IOrigin.Width - 1, IOrigin.Height - 1);

  // Аналогично для результирующего изображения
  IResult.Canvas.Pen.Color := clBlack;
  IResult.Canvas.Brush.Color := clWhite;
  IResult.Canvas.Brush.Style := bsSolid;
  IResult.Canvas.Rectangle(0, 0, IResult.Width - 1, IResult.Height - 1);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin

end;

// Событие, возникающее при двойном клике по области рисования
procedure TForm1.IOriginDblClick(Sender: TObject);
begin
  // Запускаем диалог открытия картинки и, если картинка была выбрана, загружаем её
  if OPD.Execute then
    IOrigin.Picture.LoadFromFile(OPD.FileName);
end;

{ procedure Marks(BI: array of array of boolean; Labels: array of array of byte; i,j:word; L:byte);
  begin
  if BI[i, j] and (Labels[i,j]=0) then
  begin
  labels[i,j]:= L;
  if (i > 0) then
  Marks(BI, labels, i - 1, j, L);
  if i < (M - 1) then
  Marks(BI, labels, i + 1, j, L);
  if (j > 0) then
  Marks(BI, labels, i, j - 1, L);
  if j < (N - 1) then
  Marks(BI, labels, i, j + 1, L);
  if (i>0) and (j>0)  then
  Marks(BI, labels, i-1, j-1, L);
  if (i< M-1) and (j< N-1) then
  Marks(BI, labels, i+1, j+1, L);
  if (i>0) and (j<N-1)  then
  Marks(BI, labels, i-1, j+1, L);
  if (i<M-1) and (j>0)  then
  Marks(BI, labels, i+1, j-1, L);
  end;
  end; }

end.
