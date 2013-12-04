unit UImages;

interface

type
  TPlane = array of array of byte;
  TMarkedPlane = array of array of word;

  TRGBImage = record
    N, M: word;
    R, G, B: TPlane;
  end;

  TYIQImage = record
    N, M: word;
    Y, Img, Q: TPlane;
  end;

  TBinaryImage = record
    N, M: word;
    Img: TPlane;
  end;

  TMarkedImage = record
    N, M: word;
    Img: TMarkedPlane;
  end;

procedure InitBinaryImage(var BinaryImg: TBinaryImage; N, M: word);

function GetRGBImageFromFile(FileName: string): TRGBImage;
function ConvertRGBToYIQ(RGBImg: TRGBImage): TYIQImage;
function ThresoldBinarization(Plane: TPlane; N, M: word; Thresold: byte): TBinaryImage;
function MarkBinaryImage(BI: TBinaryImage; hv, diag: boolean; obl:byte): TMarkedImage;
function Skeleton(BI: TBinaryImage): TBinaryImage;
procedure CentreOfGravity(BI: TBinaryImage; var row, col: double);

implementation

uses
  VCL.Graphics;

function ImgOR(BI1, BI2: TBinaryImage): TBinaryImage;
var
  BIR: TBinaryImage;
  I, j: word;
begin
  InitBinaryImage(BIR, BI1.N, BI1.M);
  for I := 1 to BI1.N do
    for j := 1 to BI1.M do
      BIR.Img[I, j] := BI1.Img[I, j] or BI2.Img[I, j];
  ImgOR := BIR;
end;

function ImgEquals(Img1, Img2: TBinaryImage): boolean;
var
  fl: boolean;
  I, j: word;
begin
  fl := (Img1.N = Img2.N) and (Img1.M = Img2.M);
  if fl then
    for I := 1 to Img1.N do
      for j := 1 to Img1.M do
        fl := fl and (Img1.Img[I, j] = Img2.Img[I, j]);
  ImgEquals := fl;
end;

procedure InitPlane(var Plane: TPlane; N, M: word); // объявление массива, в котором хранятся данные о цвете картинки
var
  i, j: word;
begin
  SetLength(Plane, N + 1);
  for i := 1 to N do
    SetLength(Plane[i], M + 1);
  for i := 1 to N do
    for j := 1 to M do
      Plane[i, j] := 0;
end;

procedure InitMarkedPlane(var Plane: TMarkedPlane; N, M: word); // объявление переменной, в кот.  будет хран. маркированной изобр.
var
  i, j: word;
begin
  SetLength(Plane, N + 1);
  for i := 1 to N do
    SetLength(Plane[i], M + 1);
  for i := 1 to N do
    for j := 1 to M do
      Plane[i, j] := 0;
end;

procedure InitRGBImage(var RGBImg: TRGBImage; N, M: word); // объявление переменной, в которой будет храниться изображение
begin
  RGBImg.N := N;
  RGBImg.M := M;
  InitPlane(RGBImg.R, N, M);
  InitPlane(RGBImg.G, N, M);
  InitPlane(RGBImg.B, N, M);
end;

procedure InitYIQImage(var YIQImg: TYIQImage; N, M: word); // объявление переменной, в которой будет храниться изобр. оттенков серого
begin
  YIQImg.N := N;
  YIQImg.M := M;
  InitPlane(YIQImg.Y, N, M);
  InitPlane(YIQImg.Img, N, M);
  InitPlane(YIQImg.Q, N, M);
end;

procedure InitBinaryImage(var BinaryImg: TBinaryImage; N, M: word); // объяв. переменной. в кот. хр. бинар. изобр.
begin
  BinaryImg.N := N;
  BinaryImg.M := M;
  InitPlane(BinaryImg.Img, N, M);
end;

procedure InitMarkedImage(var MarkedImg: TMarkedImage; N, M: word); // объявление переменной, в кот.  будет хран. маркированной изобр. и инф. о его высоте и шир.
begin
  MarkedImg.N := N;
  MarkedImg.M := M;
  InitMarkedPlane(MarkedImg.Img, N, M);
end;

function GetRGBImageFromFile(FileName: string): TRGBImage; // получение rgb картинки
var
  RGBImg: TRGBImage;
  BM: TBitMap;
  i, j: word;
  Color: TColor;
begin
  BM := TBitMap.Create();
  BM.LoadFromFile(FileName); // загружаем в "холст" BM выбранное изображение
  InitRGBImage(RGBImg, BM.Height, BM.Width); // объявляем переменную для rgb картинки
  for i := 1 to RGBImg.N do
    for j := 1 to RGBImg.M do
    begin
      Color := BM.Canvas.Pixels[j - 1, i - 1];
      RGBImg.R[i, j] := Color; // в каждом массиве
      RGBImg.G[i, j] := Color shr 8; // хранится данные о насыщ. определенного цвета
      RGBImg.B[i, j] := Color shr 16; // в каждом пикселе
    end;
  BM.Free;
  GetRGBImageFromFile := RGBImg;
end;

function ConvertRGBToYIQ(RGBImg: TRGBImage): TYIQImage;
var
  YIQImg: TYIQImage;
  i, j: word;
begin
  InitYIQImage(YIQImg, RGBImg.N, RGBImg.M); // объявляем переменную
  for i := 1 to YIQImg.N do
    for j := 1 to YIQImg.M do
    begin
      YIQImg.Y[i, j] := round(0.299 * RGBImg.R[i, j] + 0.587 * RGBImg.G[i, j] + 0.114 * RGBImg.B[i, j]);
      YIQImg.Img[i, j] := round(0.596 * RGBImg.R[i, j] - 0.274 * RGBImg.G[i, j] - 0.321 * RGBImg.B[i, j]);
      YIQImg.Q[i, j] := round(0.211 * RGBImg.R[i, j] - 0.523 * RGBImg.G[i, j] + 0.311 * RGBImg.B[i, j]);
    end;
  ConvertRGBToYIQ := YIQImg;
end;

function ThresoldBinarization(Plane: TPlane; N, M: word; Thresold: byte): TBinaryImage; // бинаризация
var
  BinaryImg: TBinaryImage;
  i, j: word;
begin
  InitBinaryImage(BinaryImg, N, M);
  for i := 1 to N do
    for j := 1 to M do
      if Plane[i, j] <= Thresold then // в Plane будет канал Y
        BinaryImg.Img[i, j] := 1
      else
        BinaryImg.Img[i, j] := 0;
  ThresoldBinarization := BinaryImg;
end;

function MarkBinaryImage(BI: TBinaryImage; hv, diag: boolean; obl: byte): TMarkedImage;
var
  MarkedImg: TMarkedImage;

  procedure RecursiveMark(i, j, Mark: word);
  begin
    if (i >= 1) and (i <= MarkedImg.N) and (j >= 1) and (j <= MarkedImg.M) and (MarkedImg.Img[i, j] = obl) then
    begin
      if hv then
      begin
        MarkedImg.Img[i, j] := Mark;
        RecursiveMark(i - 1, j, Mark);
        RecursiveMark(i + 1, j, Mark);
        RecursiveMark(i, j - 1, Mark);
        RecursiveMark(i, j + 1, Mark);
      end;
      if diag then // условие на поиск смежности по диагоналям
      begin
        RecursiveMark(i - 1, j - 1, Mark);
        RecursiveMark(i - 1, j + 1, Mark);
        RecursiveMark(i + 1, j - 1, Mark);
        RecursiveMark(i + 1, j + 1, Mark);
      end;
    end;
  end;

var
  i, j: word;
  Mark: word;
begin
  InitMarkedImage(MarkedImg, BI.N, BI.M);
  for i := 1 to MarkedImg.N do
    for j := 1 to MarkedImg.M do
      MarkedImg.Img[i, j] := BI.Img[i, j];
  Mark := 2;
  for i := 1 to MarkedImg.N do
    for j := 1 to MarkedImg.M do
      if MarkedImg.Img[i, j] = Obl then
      begin
        RecursiveMark(i, j, Mark);
        Mark := Mark + 1;
      end;
  for i := 1 to MarkedImg.N do
    for j := 1 to MarkedImg.M do
      if (MarkedImg.Img[i, j] > 1) and (obl=1) then
        MarkedImg.Img[i, j] := MarkedImg.Img[i, j] - 1;
  MarkBinaryImage := MarkedImg;
end;

procedure CentreOfGravity(BI: TBinaryImage; var row, col: double);
var
  i, j: word;
  s: word;
begin
  s := 0;
  row := 0;
  col := 0;
  for i := 1 to BI.N do
    for j := 1 to BI.M do
    begin
      s := s + BI.Img[i, j];
      row := row + i * BI.Img[i, j];
      col := col + j * BI.Img[i, j];
    end;
    row:=row/s;
    col:=col/s;
end;

function Skeleton(BI: TBinaryImage): TBinaryImage;
  function p(BI: TBinaryImage; r, c: word; ind: byte): byte;
  var
    res: byte;
  begin
    res := 0;
    case ind of
    0: res := BI.img[r, c];
    1: res := BI.img[r - 1, c];
    2: res := BI.img[r - 1, c + 1];
    3: res := BI.img[r, c + 1];
    4: res := BI.img[r + 1, c + 1];
    5: res := BI.img[r + 1, c];
    6: res := BI.img[r + 1, c - 1];
    7: res := BI.img[r, c - 1];
    8: res := BI.img[r - 1, c - 1];
    end;
    p := res;
  end;

  function NeighbourCount(BI: TBinaryImage; r, c: word): byte;
  var
    i: byte;
    tmp: byte;
  begin
    tmp := 0;
    for i := 1 to 8 do
      tmp := tmp + word(p(BI, r, c, i));
    NeighbourCount := tmp;
  end;

  function TransitionCount(BI: TBinaryImage; r, c: word): byte;
  var
    tmp, i: byte;
  begin
    tmp := 0;
    for i := 1 to 7 do
      if ( p(BI, r, c, i)=0) and (p(BI, r, c, i + 1)=1) then
        tmp := tmp + 1;
    if ( p(BI, r, c, 8)=0) and (p(BI, r, c, 1)=1) then
      tmp := tmp + 1;
    TransitionCount := tmp;
  end;

  function Kontur(BI: TBinaryImage): TBinaryImage;
  var
    i, j: word;
    BIR: TBinaryImage;
  begin
    InitBinaryImage(BIR, BI.N, BI.M);
    for i := 1 to BI.N do
      for j := 1 to BI.M do
        if BI.img[i, j]=1 then
          if (p(BI, i, j, 2) and p(BI, i, j, 3) and p(BI, i, j, 4) and p(BI, i, j, 5) and p(BI, i, j, 6) and p(BI, i, j, 7) and p(BI, i, j, 8)=0) then
            BIR.img[i, j] := 1;
    Kontur := BIR;
  end;

  function Thin(BIR: TBinaryImage): TBinaryImage;
  var
    Border, NewBorder: TBinaryImage;
    fl: boolean;
    i, j: word;
  begin
    Border := Kontur(BIR);
    NewBorder := ImgOR(Border, Border);
    for i := 1 to Border.N do
      for j := 1 to Border.M do
        if Border.img[i, j]=1 then
        begin
          fl := true;
          fl := fl and (NeighbourCount(BIR, i, j) in [2 .. 6]);
          fl := fl and (TransitionCount(BIR, i, j) = 1);
          fl := fl and ((p(BIR, i, j, 1) and p(BIR, i, j, 3) and p(BIR, i, j, 5)) = 0);
          fl := fl and ((p(BIR, i, j, 3) and p(BIR, i, j, 5) and p(BIR, i, j, 6)) = 0);
          if fl then
            NewBorder.img[i, j] := 0;
        end;
    for i := 1 to Border.N do
      for j := 1 to Border.M do
        if Border.img[i, j]=1 then
          BIR.img[i, j] := NewBorder.img[i, j];
    Border := Kontur(BIR);
    NewBorder := ImgOR(Border, Border);
    for i := 1 to Border.N do
      for j := 1 to Border.M do
        if Border.img[i, j]=1 then
        begin
          fl := true;
          fl := fl and (NeighbourCount(BIR, i, j) in [2 .. 6]);
          fl := fl and (TransitionCount(BIR, i, j) = 1);
          fl := fl and ((p(BIR, i, j, 1) and p(BIR, i, j, 3) and p(BIR, i, j, 7)) = 0);
          fl := fl and ((p(BIR, i, j, 1) and p(BIR, i, j, 5) and p(BIR, i, j, 7)) = 0);
          if fl then
            NewBorder.img[i, j] := 0;
        end;
    for i := 1 to Border.N do
      for j := 1 to Border.M do
        if Border.img[i, j]=1 then
          BIR.img[i, j] := NewBorder.img[i, j];
    Thin := BIR;
  end;

var
  ImgOld: TBinaryImage;
begin
  REPEAT
    ImgOld := ImgOR(BI, BI);
    BI := Thin(BI);
  UNTIL ImgEquals(BI, ImgOld);
  Skeleton := BI;
end;

end.
