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
function MarkBinaryImage(BI: TBinaryImage; hv, diag: boolean): TMarkedImage;

implementation

uses
  VCL.Graphics;

procedure InitPlane(var Plane: TPlane; N, M: word);
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

procedure InitMarkedPlane(var Plane: TMarkedPlane; N, M: word);
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

procedure InitRGBImage(var RGBImg: TRGBImage; N, M: word);
begin
  RGBImg.N := N;
  RGBImg.M := M;
  InitPlane(RGBImg.R, N, M);
  InitPlane(RGBImg.G, N, M);
  InitPlane(RGBImg.B, N, M);
end;

procedure InitYIQImage(var YIQImg: TYIQImage; N, M: word);
begin
  YIQImg.N := N;
  YIQImg.M := M;
  InitPlane(YIQImg.Y, N, M);
  InitPlane(YIQImg.Img, N, M);
  InitPlane(YIQImg.Q, N, M);
end;

procedure InitBinaryImage(var BinaryImg: TBinaryImage; N, M: word);
begin
  BinaryImg.N := N;
  BinaryImg.M := M;
  InitPlane(BinaryImg.Img, N, M);
end;

procedure InitMarkedImage(var MarkedImg: TMarkedImage; N, M: word);
begin
  MarkedImg.N := N;
  MarkedImg.M := M;
  InitMarkedPlane(MarkedImg.Img, N, M);
end;

function GetRGBImageFromFile(FileName: string): TRGBImage;
var
  RGBImg: TRGBImage;
  BM: TBitMap;
  i, j: word;
  Color: TColor;
begin
  BM := TBitMap.Create();
  BM.LoadFromFile(FileName);
  InitRGBImage(RGBImg, BM.Height, BM.Width);
  for i := 1 to RGBImg.N do
    for j := 1 to RGBImg.M do
    begin
      Color := BM.Canvas.Pixels[j - 1, i - 1];
      RGBImg.R[i, j] := Color;
      RGBImg.G[i, j] := Color shr 8;
      RGBImg.B[i, j] := Color shr 16;
    end;
  BM.Free;
  GetRGBImageFromFile := RGBImg;
end;

function ConvertRGBToYIQ(RGBImg: TRGBImage): TYIQImage;
var
  YIQImg: TYIQImage;
  i, j: word;
begin
  InitYIQImage(YIQImg, RGBImg.N, RGBImg.M);
  for i := 1 to YIQImg.N do
    for j := 1 to YIQImg.M do
    begin
      YIQImg.Y[i, j] := round(0.299 * RGBImg.R[i, j] + 0.587 * RGBImg.G[i, j] + 0.114 * RGBImg.B[i, j]);
      // TODO —формировать остальные каналы
      YIQImg.Img[i, j] := 0;
      YIQImg.Q[i, j] := 0;
    end;
  ConvertRGBToYIQ := YIQImg;
end;

function ThresoldBinarization(Plane: TPlane; N, M: word; Thresold: byte): TBinaryImage;
var
  BinaryImg: TBinaryImage;
  i, j: word;
begin
  InitBinaryImage(BinaryImg, N, M);
  for i := 1 to N do
    for j := 1 to M do
      if Plane[i, j] <= Thresold then
        BinaryImg.Img[i, j] := 1
      else
        BinaryImg.Img[i, j] := 0;
  ThresoldBinarization := BinaryImg;
end;

function MarkBinaryImage(BI: TBinaryImage; hv, diag: boolean): TMarkedImage;
var
  MarkedImg: TMarkedImage;

  procedure RecursiveMark(i, j, Mark: word);
  begin
    if (i >= 1) and (i <= MarkedImg.N) and (j >= 1) and (j <= MarkedImg.M) and (MarkedImg.Img[i, j] = 1) then
    begin
      if hv then
      begin
        MarkedImg.Img[i, j] := Mark;
        RecursiveMark(i - 1, j, Mark);
        RecursiveMark(i + 1, j, Mark);
        RecursiveMark(i, j - 1, Mark);
        RecursiveMark(i, j + 1, Mark);
      end;
      if diag then
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
      if MarkedImg.Img[i, j] = 1 then
      begin
        RecursiveMark(i, j, Mark);
        Mark := Mark + 1;
      end;
  for i := 1 to MarkedImg.N do
    for j := 1 to MarkedImg.M do
      if MarkedImg.Img[i, j] > 1 then
        MarkedImg.Img[i, j] := MarkedImg.Img[i, j] - 1;
  MarkBinaryImage := MarkedImg;
end;

end.
