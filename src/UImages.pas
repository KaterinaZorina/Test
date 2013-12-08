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
procedure InitMarkedImage(var MarkedImg: TMarkedImage; N, M: word); // объявление переменной, в кот.  будет хран. маркированной изобр. и инф. о его высоте и шир.
function GetRGBImageFromFile(FileName: string): TRGBImage;
function ConvertRGBToYIQ(RGBImg: TRGBImage): TYIQImage;
function ThresoldBinarization(Plane: TPlane; N, M: word; Thresold: byte): TBinaryImage;
function MarkBinaryImage(BI: TBinaryImage; hv, diag: boolean; obl: byte): TMarkedImage;
function Skeleton(BI: TBinaryImage): TBinaryImage;
procedure CentreOfGravity(BI: TMarkedImage; var row, col: double; mark:byte);

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
  I, j: word;
begin
  SetLength(Plane, N + 1);
  for I := 1 to N do
    SetLength(Plane[I], M + 1);
  for I := 1 to N do
    for j := 1 to M do
      Plane[I, j] := 0;
end;

procedure InitMarkedPlane(var Plane: TMarkedPlane; N, M: word); // объявление переменной, в кот.  будет хран. маркированной изобр.
var
  I, j: word;
begin
  SetLength(Plane, N + 1);
  for I := 1 to N do
    SetLength(Plane[I], M + 1);
  for I := 1 to N do
    for j := 1 to M do
      Plane[I, j] := 0;
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
  I, j: word;
  Color: TColor;
begin
  BM := TBitMap.Create();
  BM.LoadFromFile(FileName); // загружаем в "холст" BM выбранное изображение
  InitRGBImage(RGBImg, BM.Height, BM.Width); // объявляем переменную для rgb картинки
  for I := 1 to RGBImg.N do
    for j := 1 to RGBImg.M do
    begin
      Color := BM.Canvas.Pixels[j - 1, I - 1];
      RGBImg.R[I, j] := Color; // в каждом массиве
      RGBImg.G[I, j] := Color shr 8; // хранится данные о насыщ. определенного цвета
      RGBImg.B[I, j] := Color shr 16; // в каждом пикселе
    end;
  BM.Free;
  GetRGBImageFromFile := RGBImg;
end;

function ConvertRGBToYIQ(RGBImg: TRGBImage): TYIQImage;
var
  YIQImg: TYIQImage;
  I, j: word;
begin
  InitYIQImage(YIQImg, RGBImg.N, RGBImg.M); // объявляем переменную
  for I := 1 to YIQImg.N do
    for j := 1 to YIQImg.M do
    begin
      YIQImg.Y[I, j] := round(0.299 * RGBImg.R[I, j] + 0.587 * RGBImg.G[I, j] + 0.114 * RGBImg.B[I, j]);
      YIQImg.Img[I, j] := round(0.596 * RGBImg.R[I, j] - 0.274 * RGBImg.G[I, j] - 0.321 * RGBImg.B[I, j]);
      YIQImg.Q[I, j] := round(0.211 * RGBImg.R[I, j] - 0.523 * RGBImg.G[I, j] + 0.311 * RGBImg.B[I, j]);
    end;
  ConvertRGBToYIQ := YIQImg;
end;

function ThresoldBinarization(Plane: TPlane; N, M: word; Thresold: byte): TBinaryImage; // бинаризация
var
  BinaryImg: TBinaryImage;
  I, j: word;
begin
  InitBinaryImage(BinaryImg, N, M);
  for I := 1 to N do
    for j := 1 to M do
      if Plane[I, j] <= Thresold then // в Plane будет канал Y
        BinaryImg.Img[I, j] := 1
      else
        BinaryImg.Img[I, j] := 0;
  ThresoldBinarization := BinaryImg;
end;

function MarkBinaryImage(BI: TBinaryImage; hv, diag: boolean; obl: byte): TMarkedImage;
var
  MarkedImg: TMarkedImage;

  procedure RecursiveMark(I, j, Mark: word);
  begin
    if (I >= 1) and (I <= MarkedImg.N) and (j >= 1) and (j <= MarkedImg.M) and (MarkedImg.Img[I, j] = obl) then
    begin
      if hv then
      begin
        MarkedImg.Img[I, j] := Mark;
        RecursiveMark(I - 1, j, Mark);
        RecursiveMark(I + 1, j, Mark);
        RecursiveMark(I, j - 1, Mark);
        RecursiveMark(I, j + 1, Mark);
      end;
      if diag then // условие на поиск смежности по диагоналям
      begin
        RecursiveMark(I - 1, j - 1, Mark);
        RecursiveMark(I - 1, j + 1, Mark);
        RecursiveMark(I + 1, j - 1, Mark);
        RecursiveMark(I + 1, j + 1, Mark);
      end;
    end;
  end;

var
  I, j: word;
  Mark: word;
begin
  InitMarkedImage(MarkedImg, BI.N, BI.M);
  for I := 1 to MarkedImg.N do
    for j := 1 to MarkedImg.M do
      MarkedImg.Img[I, j] := BI.Img[I, j];
  Mark := 2;
  for I := 1 to MarkedImg.N do
    for j := 1 to MarkedImg.M do
      if MarkedImg.Img[I, j] = obl then
      begin
        RecursiveMark(I, j, Mark);
        Mark := Mark + 1;
      end;
  for I := 1 to MarkedImg.N do
    for j := 1 to MarkedImg.M do
      if (MarkedImg.Img[I, j] > 1) and (obl = 1) then
        MarkedImg.Img[I, j] := MarkedImg.Img[I, j] - 1;
  MarkBinaryImage := MarkedImg;
end;

procedure CentreOfGravity(BI: TMarkedImage; var row, col: double; mark:byte);
var
  I, j: word;
  s: word;
begin
  s := 0;
  row := 0;
  col := 0;
  for I := 1 to BI.N do
    for j := 1 to BI.M do
    begin
      if BI.Img[i,j]=mark then
      begin
      s := s + 1;
      row := row + I;
      col := col + j;
      end;
    end;
  row := row / s;
  col := col / s;
end;

function Skeleton(BI: TBinaryImage): TBinaryImage;
  function p(BI: TBinaryImage; R, c: word; ind: byte): byte;
  var
    res: byte;
  begin
    res := 0;
    case ind of
    0: res := BI.Img[R, c];
    1: res := BI.Img[R - 1, c];
    2: res := BI.Img[R - 1, c + 1];
    3: res := BI.Img[R, c + 1];
    4: res := BI.Img[R + 1, c + 1];
    5: res := BI.Img[R + 1, c];
    6: res := BI.Img[R + 1, c - 1];
    7: res := BI.Img[R, c - 1];
    8: res := BI.Img[R - 1, c - 1];
    end;
    p := res;
  end;
  function NeighbourCount(BI: TBinaryImage; R, c: word): byte;
  var
    I: byte;
    tmp: byte;
  begin
    tmp := 0;
    for I := 1 to 8 do
      tmp := tmp + word(p(BI, R, c, I));
    NeighbourCount := tmp;
  end;
  function TransitionCount(BI: TBinaryImage; R, c: word): byte;
  var
    tmp, I: byte;
  begin
    tmp := 0;
    for I := 1 to 7 do
      if (p(BI, R, c, I) = 0) and (p(BI, R, c, I + 1) = 1) then
        tmp := tmp + 1;
    if (p(BI, R, c, 8) = 0) and (p(BI, R, c, 1) = 1) then
      tmp := tmp + 1;
    TransitionCount := tmp;
  end;
  function Kontur(BI: TBinaryImage): TBinaryImage;
  var
    I, j: word;
    BIR: TBinaryImage;
  begin
    InitBinaryImage(BIR, BI.N, BI.M);
    for I := 1 to BI.N do
      for j := 1 to BI.M do
        if BI.Img[I, j] = 1 then
          if (p(BI, I, j, 2) and p(BI, I, j, 3) and p(BI, I, j, 4) and p(BI, I, j, 5) and p(BI, I, j, 6) and p(BI, I, j, 7) and p(BI, I, j, 8) = 0) then
            BIR.Img[I, j] := 1;
    Kontur := BIR;
  end;
  function Thin(BIR: TBinaryImage): TBinaryImage;
  var
    Border, NewBorder: TBinaryImage;
    fl: boolean;
    I, j: word;
  begin
    Border := Kontur(BIR);
    NewBorder := ImgOR(Border, Border);
    for I := 1 to Border.N do
      for j := 1 to Border.M do
        if Border.Img[I, j] = 1 then
        begin
          fl := true;
          fl := fl and (NeighbourCount(BIR, I, j) in [2 .. 6]);
          fl := fl and (TransitionCount(BIR, I, j) = 1);
          fl := fl and ((p(BIR, I, j, 1) and p(BIR, I, j, 3) and p(BIR, I, j, 5)) = 0);
          fl := fl and ((p(BIR, I, j, 3) and p(BIR, I, j, 5) and p(BIR, I, j, 6)) = 0);
          if fl then
            NewBorder.Img[I, j] := 0;
        end;
    for I := 1 to Border.N do
      for j := 1 to Border.M do
        if Border.Img[I, j] = 1 then
          BIR.Img[I, j] := NewBorder.Img[I, j];
    Border := Kontur(BIR);
    NewBorder := ImgOR(Border, Border);
    for I := 1 to Border.N do
      for j := 1 to Border.M do
        if Border.Img[I, j] = 1 then
        begin
          fl := true;
          fl := fl and (NeighbourCount(BIR, I, j) in [2 .. 6]);
          fl := fl and (TransitionCount(BIR, I, j) = 1);
          fl := fl and ((p(BIR, I, j, 1) and p(BIR, I, j, 3) and p(BIR, I, j, 7)) = 0);
          fl := fl and ((p(BIR, I, j, 1) and p(BIR, I, j, 5) and p(BIR, I, j, 7)) = 0);
          if fl then
            NewBorder.Img[I, j] := 0;
        end;
    for I := 1 to Border.N do
      for j := 1 to Border.M do
        if Border.Img[I, j] = 1 then
          BIR.Img[I, j] := NewBorder.Img[I, j];
    Thin := BIR;
  end;

var
  ImgOld: TBinaryImage;
begin
  repeat
    ImgOld := ImgOR(BI, BI);
    BI := Thin(BI);
  until ImgEquals(BI, ImgOld);
  Skeleton := BI;
end;

end.
