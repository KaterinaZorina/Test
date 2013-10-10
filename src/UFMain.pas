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

implementation

{$R *.dfm}

uses
  UImages;

// Событие OnActivate выполняется в момент первой активации формы
procedure TForm1.BGetGreyscaleClick(Sender: TObject);
var
  RGB: TRGBImage;
  YIQ: TYIQImage;
  BI: TBinaryImage;
  MarkedImg: TMarkedImage;
  BM: TBitMap;
  i, j: word;
begin
  RGB := UImages.GetRGBImageFromFile(OPD.FileName);
  YIQ := UImages.ConvertRGBToYIQ(RGB);
  BI := UImages.ThresoldBinarization(YIQ.Y, YIQ.N, YIQ.M, strtoint(Ethresold.Text));
  MarkedImg := MarkBinaryImage(BI);

  BM := TBitMap.Create;
  BM.Height := MarkedImg.N;
  BM.Width := MarkedImg.M;
  for i := 0 to MarkedImg.N - 1 do
    for j := 0 to MarkedImg.M - 1 do
    begin
      if MarkedImg.i[i + 1, j + 1] = 0 then
        BM.Canvas.Pixels[j, i] := clWhite
      else
        case (MarkedImg.i[i + 1, j + 1] mod 15) + 1 of
        1: BM.Canvas.Pixels[j, i] := clAqua;
        2: BM.Canvas.Pixels[j, i] := clBlack;
        3: BM.Canvas.Pixels[j, i] := clBlue;
        4: BM.Canvas.Pixels[j, i] := clFuchsia;
        5: BM.Canvas.Pixels[j, i] := clGray;
        6: BM.Canvas.Pixels[j, i] := clGreen;
        7: BM.Canvas.Pixels[j, i] := clLime;
        8: BM.Canvas.Pixels[j, i] := clMaroon;
        9: BM.Canvas.Pixels[j, i] := clNavy;
        10: BM.Canvas.Pixels[j, i] := clOlive;
        11: BM.Canvas.Pixels[j, i] := clPurple;
        12: BM.Canvas.Pixels[j, i] := clRed;
        13: BM.Canvas.Pixels[j, i] := clSilver;
        14: BM.Canvas.Pixels[j, i] := clTeal;
        15: BM.Canvas.Pixels[j, i] := clYellow;
        end;
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
