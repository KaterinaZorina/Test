object Form1: TForm1
  Left = 0
  Top = 0
  Caption = #1058#1077#1089#1090#1086#1074#1072#1103' '#1092#1086#1088#1084#1072
  ClientHeight = 311
  ClientWidth = 552
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnActivate = FormActivate
  PixelsPerInch = 96
  TextHeight = 13
  object IOrigin: TImage
    Left = 8
    Top = 8
    Width = 265
    Height = 265
    Hint = #1044#1074#1072#1078#1076#1099' '#1082#1083#1080#1082#1085#1080' '#1085#1072' '#1084#1085#1077
    Center = True
    ParentShowHint = False
    Proportional = True
    ShowHint = True
    Stretch = True
    OnDblClick = IOriginDblClick
  end
  object IResult: TImage
    Left = 279
    Top = 8
    Width = 265
    Height = 265
    Center = True
    ParentShowHint = False
    Proportional = True
    ShowHint = False
    Stretch = True
  end
  object BGetGreyscale: TButton
    Left = 8
    Top = 279
    Width = 536
    Height = 25
    Caption = 'BGetGreyscale'
    TabOrder = 0
    OnClick = BGetGreyscaleClick
  end
  object OPD: TOpenPictureDialog
    Filter = 'Bitmaps (*.bmp)|*.bmp'
    Left = 16
    Top = 16
  end
end
