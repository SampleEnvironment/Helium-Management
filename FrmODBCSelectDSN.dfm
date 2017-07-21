object FormODBCSelectDSN: TFormODBCSelectDSN
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsDialog
  Caption = 'ODBC (32Bit) :: data source names'
  ClientHeight = 299
  ClientWidth = 252
  Color = clWhite
  Ctl3D = False
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object PanelBtns: TPanel
    Left = 0
    Top = 258
    Width = 252
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    Color = clWhite
    Ctl3D = False
    ParentCtl3D = False
    TabOrder = 0
    DesignSize = (
      252
      41)
    object FlatBtnCancel: TFlatBtn
      Left = 171
      Top = 6
      Width = 73
      Height = 25
      Caption = 'Cancel'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -10
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      Anchors = [akRight, akBottom]
      BorderRadius = 8
      Color = 16053492
      ParentFont = False
      Shadow = 10
      ShadowRadius = 50
      OnClick = FlatBtnCancelClick
      ExplicitLeft = 345
    end
    object FlatBtnOk: TFlatBtn
      Left = 92
      Top = 6
      Width = 73
      Height = 25
      Caption = 'Ok'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -10
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      Anchors = [akRight, akBottom]
      BorderRadius = 8
      Color = 16053492
      ParentFont = False
      Shadow = 10
      ShadowRadius = 50
      OnClick = FlatBtnOkClick
      ExplicitLeft = 266
    end
  end
  object PanelMain: TPanel
    Left = 0
    Top = 0
    Width = 252
    Height = 235
    Align = alClient
    BevelOuter = bvNone
    Color = clWhite
    Ctl3D = False
    ParentCtl3D = False
    TabOrder = 1
    object LBDSNs: TListBox
      Left = 0
      Top = 0
      Width = 252
      Height = 235
      Align = alClient
      ItemHeight = 13
      TabOrder = 0
      OnClick = LBDSNsClick
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 235
    Width = 252
    Height = 23
    Align = alBottom
    BevelOuter = bvNone
    Color = clWhite
    TabOrder = 2
    DesignSize = (
      252
      23)
    object Label1: TLabel
      Left = 8
      Top = 6
      Width = 64
      Height = 13
      Caption = 'Current DSN:'
    end
    object LabCurrentDSN: TLabel
      Left = 78
      Top = 6
      Width = 74
      Height = 13
      Anchors = [akTop, akRight]
      Caption = 'LabCurrentDSN'
    end
  end
end
