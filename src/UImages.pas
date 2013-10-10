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
    Y, I, Q: TPlane;
  end;

  TBinaryImage = record
    N, M: word;
    I: TPlane;
  end;

  TMarkedImage = record
    N, M: word;
    I: TMarkedPlane;
  end;

function GetRGBImageFromFile(FileName: string): TRGBImage;
function ConvertRGBToYIQ(RGBImg: TRGBImage): TYIQImage;
function ThresoldBinarization(Plane: TPlane; N, M: word; Thresold: byte): TBinaryImage;
function MarkBinaryImage(BI: TBinaryImage): TMarkedImage;

implementation

uses
  VCL.Graphics;

procedure InitPlane(var Plane: TPlane; N, M: word);
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

procedure InitMarkedPlane(var Plane: TMarkedPlane; N, M: word);
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
  InitPlane(YIQImg.I, N, M);
  InitPlane(YIQImg.Q, N, M);
end;

procedure InitBinaryImage(var BinaryImg: TBinaryImage; N, M: word);
begin
  BinaryImg.N := N;
  BinaryImg.M := M;
  InitPlane(BinaryImg.I, N, M);
end;

procedure InitMarkedImage(var MarkedImg: TMarkedImage; N, M: word);
begin
  MarkedImg.N := N;
  MarkedImg.M := M;
  InitMarkedPlane(MarkedImg.I, N, M);
end;

function GetRGBImageFromFile(FileName: string): TRGBImage;
var
  RGBImg: TRGBImage;
  BM: TBitMap;
  I, j: word;
  Color: TColor;
begin
  BM := TBitMap.Create();
  BM.LoadFromFile(FileName);
  InitRGBImage(RGBImg, BM.Height, BM.Width);
  for I := 1 to RGBImg.N do
    for j := 1 to RGBImg.M do
    begin
      Color := BM.Canvas.Pixels[j - 1, I - 1];
      RGBImg.R[I, j] := Color;
      RGBImg.G[I, j] := Color shr 8;
      RGBImg.B[I, j] := Color shr 16;
    end;
  BM.Free;
  GetRGBImageFromFile := RGBImg;
end;

function ConvertRGBToYIQ(RGBImg: TRGBImage): TYIQImage;
var
  YIQImg: TYIQImage;
  I, j: word;
begin
  InitYIQImage(YIQImg, RGBImg.N, RGBImg.M);
  for I := 1 to YIQImg.N do
    for j := 1 to YIQImg.M do
    begin
      YIQImg.Y[I, j] := round(0.299 * RGBImg.R[I, j] + 0.587 * RGBImg.G[I, j] + 0.114 * RGBImg.B[I, j]);
      // TODO —формировать остальные каналы
      YIQImg.I[I, j] := 0;
      YIQImg.Q[I, j] := 0;
    end;
  ConvertRGBToYIQ := YIQImg;
end;

function ThresoldBinarization(Plane: TPlane; N, M: word; Thresold: byte): TBinaryImage;
var
  BinaryImg: TBinaryImage;
  I, j: word;
begin
  InitBinaryImage(BinaryImg, N, M);
  for I := 1 to N do
    for j := 1 to M do
      if Plane[I, j] <= Thresold then
        BinaryImg.I[I, j] := 1
      else
        BinaryImg.I[I, j] := 0;
  ThresoldBinarization := BinaryImg;
end;

function MarkBinaryImage(BI: TBinaryImage): TMarkedImage;
var
  MarkedImg: TMarkedImage;

  procedure RecursiveMark(I, j, Mark: word);
  begin
    if (I >= 1) and (I <= MarkedImg.N) and (j >= 1) and (j <= MarkedImg.M) and (MarkedImg.I[I, j] = 1) then
    begin
      MarkedImg.I[I, j] := Mark;
      RecursiveMark(I - 1, j, Mark);
      RecursiveMark(I + 1, j, Mark);
      RecursiveMark(I, j - 1, Mark);
      RecursiveMark(I, j + 1, Mark);
    end;
  end;

var
  I, j: word;
  Mark: word;
begin
  InitMarkedImage(MarkedImg, BI.N, BI.M);
  for I := 1 to MarkedImg.N do
    for j := 1 to MarkedImg.M do
      MarkedImg.I[I, j] := BI.I[I, j];

  Mark := 2;
  for I := 1 to MarkedImg.N do
    for j := 1 to MarkedImg.M do
      if MarkedImg.I[I, j] = 1 then
      begin
        RecursiveMark(I, j, Mark);
        Mark := Mark + 1;
      end;

  for I := 1 to MarkedImg.N do
    for j := 1 to MarkedImg.M do
      if MarkedImg.I[I, j] > 1 then
        MarkedImg.I[I, j] := MarkedImg.I[I, j] - 1;

  MarkBinaryImage := MarkedImg;
end;

end.
