object Form1: TForm1
  Left = 0
  Top = 0
  Caption = #1056#1072#1089#1087#1086#1079#1085#1072#1074#1072#1085#1080#1077' '#1089#1080#1084#1074#1086#1083#1086#1074
  ClientHeight = 442
  ClientWidth = 836
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnActivate = FormActivate
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object IOrigin: TImage
    Left = 8
    Top = 8
    Width = 385
    Height = 385
    Hint = #1044#1074#1072#1078#1076#1099' '#1082#1083#1080#1082#1085#1080#1090#1077' '#1076#1083#1103' '#1079#1072#1075#1088#1091#1079#1082#1080' '#1080#1079#1086#1073#1088#1072#1078#1077#1085#1080#1103
    Center = True
    ParentShowHint = False
    Proportional = True
    ShowHint = True
    Stretch = True
    OnDblClick = IOriginDblClick
  end
  object IResult: TImage
    Left = 399
    Top = 8
    Width = 383
    Height = 385
    Center = True
    ParentShowHint = False
    Proportional = True
    ShowHint = False
    Stretch = True
    OnDblClick = IResultDblClick
  end
  object Label1: TLabel
    Left = 788
    Top = 8
    Width = 31
    Height = 13
    Caption = 'Label1'
  end
  object Label2: TLabel
    Left = 788
    Top = 27
    Width = 31
    Height = 13
    Caption = 'Label2'
  end
  object BMarkImage: TButton
    Left = 55
    Top = 409
    Width = 338
    Height = 25
    Caption = #1052#1072#1088#1082#1080#1088#1086#1074#1082#1072' '#1080#1079#1086#1073#1088#1072#1078#1077#1085#1080#1103
    TabOrder = 0
    OnClick = BMarkImageClick
  end
  object BShowMark: TButton
    Left = 446
    Top = 409
    Width = 336
    Height = 25
    Caption = #1055#1086#1082#1072#1079#1072#1090#1100' '#1086#1073#1098#1077#1082#1090
    TabOrder = 1
    OnClick = BShowMarkClick
  end
  object LEThresold: TLabeledEdit
    Left = 8
    Top = 411
    Width = 41
    Height = 21
    EditLabel.Width = 34
    EditLabel.Height = 13
    EditLabel.Caption = #1055#1086#1088#1086#1075':'
    TabOrder = 2
    Text = '100'
  end
  object LEMark: TLabeledEdit
    Left = 399
    Top = 411
    Width = 41
    Height = 21
    EditLabel.Width = 36
    EditLabel.Height = 13
    EditLabel.Caption = #1052#1077#1090#1082#1072':'
    TabOrder = 3
    Text = '1'
  end
  object Obj: TLabeledEdit
    Left = 787
    Top = 75
    Width = 41
    Height = 21
    EditLabel.Width = 36
    EditLabel.Height = 13
    EditLabel.Caption = #1052#1077#1090#1082#1072':'
    TabOrder = 4
    Text = '1'
  end
  object CoG: TButton
    Left = 788
    Top = 102
    Width = 43
    Height = 25
    Caption = #1062#1058
    TabOrder = 5
    OnClick = BShowMarkClick
  end
  object OPD: TOpenPictureDialog
    Filter = 'Bitmaps (*.bmp)|*.bmp'
    Left = 16
    Top = 16
  end
end
