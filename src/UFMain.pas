unit UFMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtDlgs, Vcl.ExtCtrls, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    IOrigin: TImage;
    OPD: TOpenPictureDialog;
    BMarkImage: TButton;
    IResult: TImage;
    BShowMark: TButton;
    LEThresold: TLabeledEdit;
    LEMark: TLabeledEdit;
    Label1: TLabel;
    Label2: TLabel;
    procedure FormActivate(Sender: TObject);
    procedure IOriginDblClick(Sender: TObject);
    procedure BMarkImageClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure IResultDblClick(Sender: TObject);
    procedure BShowMarkClick(Sender: TObject);
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

var
  MarkedImg: TMarkedImage;

  // Событие OnActivate выполняется в момент первой активации формы
procedure TForm1.BMarkImageClick(Sender: TObject);
var
  RGB: TRGBImage;
  YIQ: TYIQImage;
  BI: TBinaryImage;
  BM: TBitMap;
  i, j: word;
begin
  RGB := UImages.GetRGBImageFromFile(OPD.FileName);
  YIQ := UImages.ConvertRGBToYIQ(RGB);
  BI := UImages.ThresoldBinarization(YIQ.Y, YIQ.N, YIQ.M, strtoint(LEThresold.Text));
  MarkedImg := MarkBinaryImage(BI, true, true);

  BM := TBitMap.Create;
  BM.Height := MarkedImg.N;
  BM.Width := MarkedImg.M;
  for i := 0 to MarkedImg.N - 1 do
    for j := 0 to MarkedImg.M - 1 do
    begin
      if MarkedImg.Img[i + 1, j + 1] = 0 then
        BM.Canvas.Pixels[j, i] := clWhite
      else
        case (MarkedImg.Img[i + 1, j + 1] mod 15) + 1 { зачем mod 15 и +1? } of
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

procedure TForm1.BShowMarkClick(Sender: TObject);
var
  num: word;
  i, j: word;
  sr, er, sc, ec: word;
  BI: UImages.TBinaryImage;
  BM: TBitMap;
  r, c: double;
begin
  num := strtoint(LEMark.Text);
  sr := MarkedImg.N;
  sc := MarkedImg.M;
  er := 1;
  ec := 1;
  for i := 1 to MarkedImg.N do
    for j := 1 to MarkedImg.M do
      if MarkedImg.Img[i, j] = num then
      begin
        if i < sr then
          sr := i;
        if i > er then
          er := i;
        if j < sc then
          sc := j;
        if j > ec then
          ec := j;
      end;
  UImages.InitBinaryImage(BI, er - sr + 1 + 2, ec - sc + 1 + 2); // переменная под вырезаемый объект на изображении
  for i := 2 to BI.N - 1 do
    for j := 2 to BI.M - 1 do
      if MarkedImg.Img[sr + i - 2, sc + j - 2] <> 0 then // копируем объект в новую переменную
        BI.Img[i, j] := 1;
  UImages.CentreOfGravity(BI, r, c);
  BI:=skeleton(BI);
  Sleep(1);
  BM := TBitMap.Create;
  BM.Height := BI.N;
  BM.Width := BI.M;
  for i := 0 to BI.N - 1 do
    for j := 0 to BI.M - 1 do
    begin
      if BI.Img[i + 1, j + 1] = 0 then
        BM.Canvas.Pixels[j, i] := clWhite
      else
        BM.Canvas.Pixels[j, i] := clBlack;
      if (round(r) = i + 1) or (round(c) = j + 1) then
        BM.Canvas.Pixels[j, i] := clRed;
    end;
  IResult.Picture.Assign(BM);
  BM.Free;
  Label1.Caption := floattostrf(r / BI.N, ffFixed, 3, 5);
  Label2.Caption := floattostrf(c / BI.M, ffFixed, 3, 5);
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

procedure TForm1.IResultDblClick(Sender: TObject);
begin
  IResult.Picture.SaveToFile('IResult.bmp');
end;

end.
