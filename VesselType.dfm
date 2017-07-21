object VesselTypeForm: TVesselTypeForm
  Left = 0
  Top = 0
  BorderStyle = bsSingle
  Caption = 'Helium Management :: Vessel Settings'
  ClientHeight = 142
  ClientWidth = 433
  Color = clHighlightText
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object HeadLab: TLabel
    Left = 24
    Top = 18
    Width = 260
    Height = 15
    Caption = 'Select a vessel to adapt the settings for vessel '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = 15
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object TypeLab: TLabel
    Left = 240
    Top = 50
    Width = 22
    Height = 11
    Caption = 'Type'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object NameLab: TLabel
    Left = 24
    Top = 50
    Width = 67
    Height = 11
    Caption = 'Vessel (number)'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object NameCoBox: TComboBox
    Left = 16
    Top = 62
    Width = 185
    Height = 21
    Style = csDropDownList
    ItemHeight = 0
    TabOrder = 0
  end
  object TypeEd: TEdit
    Left = 232
    Top = 62
    Width = 185
    Height = 21
    Enabled = False
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
  end
  object OkBtn: TButton
    Left = 254
    Top = 109
    Width = 75
    Height = 25
    Caption = 'Ok'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 2
    OnClick = OkBtnClick
  end
  object CancelBtn: TButton
    Left = 335
    Top = 109
    Width = 75
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
end
