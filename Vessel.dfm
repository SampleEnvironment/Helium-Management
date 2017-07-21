object VesselForm: TVesselForm
  Left = 0
  Top = 0
  BorderStyle = bsSingle
  Caption = 'Helium Management :: Vessel'
  ClientHeight = 592
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
  object CopyLab: TLabel
    Left = 24
    Top = 488
    Width = 42
    Height = 13
    Cursor = crHandPoint
    Caption = 'CopyLab'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clActiveCaption
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsUnderline]
    ParentFont = False
    OnClick = CopyLabClick
  end
  object HintLab: TLabel
    Left = 24
    Top = 512
    Width = 43
    Height = 13
    Caption = 'HintLab'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Comment2Lab: TLabel
    Left = 240
    Top = 362
    Width = 41
    Height = 11
    Caption = 'Comment'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object AdcLab: TLabel
    Left = 240
    Top = 320
    Width = 122
    Height = 11
    Caption = 'ADC Loop (number of cycles)'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object MaxResLab: TLabel
    Left = 240
    Top = 278
    Width = 90
    Height = 11
    Caption = 'Max Resistance (ohm)'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object LongIntLab: TLabel
    Left = 240
    Top = 236
    Width = 79
    Height = 11
    Caption = 'Long Interval (min)'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Comment1Lab: TLabel
    Left = 240
    Top = 88
    Width = 41
    Height = 11
    Caption = 'Comment'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object ZeroLab: TLabel
    Left = 24
    Top = 446
    Width = 48
    Height = 11
    Caption = 'Zero (mbar)'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object SpanLab: TLabel
    Left = 24
    Top = 404
    Width = 70
    Height = 11
    Caption = 'Span (mbar/volt)'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object FilltimeLab: TLabel
    Left = 24
    Top = 362
    Width = 73
    Height = 11
    Caption = 'Fill Timeout (min)'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object HeatLab: TLabel
    Left = 24
    Top = 320
    Width = 74
    Height = 11
    Caption = 'Heat Time (msec)'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object MinResLab: TLabel
    Left = 24
    Top = 278
    Width = 88
    Height = 11
    Caption = 'Min Resistance (ohm)'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object ShortIntLab: TLabel
    Left = 24
    Top = 236
    Width = 80
    Height = 11
    Caption = 'Short Interval (sec)'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object TareLab: TLabel
    Left = 24
    Top = 172
    Width = 38
    Height = 11
    Caption = 'Tare (kg)'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object VolumeLab: TLabel
    Left = 24
    Top = 130
    Width = 43
    Height = 11
    Caption = 'Volume (l)'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object TypeLab: TLabel
    Left = 24
    Top = 88
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
    Top = 18
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
  object DeleteBtn: TButton
    Left = 334
    Top = 521
    Width = 75
    Height = 25
    Caption = 'Delete'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 18
    OnClick = DeleteBtnClick
  end
  object SaveBtn: TButton
    Left = 334
    Top = 490
    Width = 75
    Height = 25
    Caption = 'Save'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 17
    OnClick = SaveBtnClick
  end
  object Comment2RiEd: TRichEdit
    Left = 232
    Top = 374
    Width = 185
    Height = 105
    Lines.Strings = (
      'RichEdit1')
    TabOrder = 16
  end
  object AdcEd: TOraEdit
    Left = 232
    Top = 332
    Width = 185
    Height = 21
    MaxLength = 3
    TabOrder = 12
    InsertMode = imInteger
  end
  object MaxResEd: TOraEdit
    Left = 232
    Top = 290
    Width = 185
    Height = 21
    MaxLength = 6
    TabOrder = 10
    InsertMode = imFloat
  end
  object LongIntEd: TOraEdit
    Left = 232
    Top = 248
    Width = 185
    Height = 21
    MaxLength = 5
    TabOrder = 8
    InsertMode = imInteger
  end
  object Comment1RiEd: TRichEdit
    Left = 232
    Top = 100
    Width = 185
    Height = 105
    Enabled = False
    Lines.Strings = (
      'RichEdit1')
    TabOrder = 5
  end
  object ZeroEd: TOraEdit
    Left = 16
    Top = 458
    Width = 185
    Height = 21
    MaxLength = 5
    TabOrder = 15
    InsertMode = imInteger
  end
  object SpanEd: TOraEdit
    Left = 16
    Top = 416
    Width = 185
    Height = 21
    MaxLength = 5
    TabOrder = 14
    InsertMode = imInteger
  end
  object FilltimeEd: TOraEdit
    Left = 16
    Top = 374
    Width = 185
    Height = 21
    MaxLength = 3
    TabOrder = 13
    InsertMode = imInteger
  end
  object HeatEd: TOraEdit
    Left = 16
    Top = 332
    Width = 185
    Height = 21
    MaxLength = 5
    TabOrder = 11
    InsertMode = imInteger
  end
  object MinResEd: TOraEdit
    Left = 16
    Top = 290
    Width = 185
    Height = 21
    MaxLength = 6
    TabOrder = 9
    InsertMode = imFloat
  end
  object ShortIntEd: TOraEdit
    Left = 16
    Top = 248
    Width = 185
    Height = 21
    MaxLength = 5
    TabOrder = 7
    InsertMode = imInteger
  end
  object Panel2: TPanel
    Left = 16
    Top = 220
    Width = 401
    Height = 2
    TabOrder = 6
  end
  object TareEd: TEdit
    Left = 16
    Top = 184
    Width = 185
    Height = 21
    Enabled = False
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 4
  end
  object VolumeEd: TEdit
    Left = 16
    Top = 142
    Width = 185
    Height = 21
    Enabled = False
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 3
  end
  object TypeEd: TEdit
    Left = 16
    Top = 100
    Width = 185
    Height = 21
    Enabled = False
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
  end
  object VesselCoBox: TComboBox
    Left = 16
    Top = 30
    Width = 185
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 0
    OnChange = VesselCoBoxChange
  end
  object Panel1: TPanel
    Left = 16
    Top = 71
    Width = 401
    Height = 2
    TabOrder = 1
  end
  object CancelBtn: TButton
    Left = 334
    Top = 560
    Width = 75
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 19
  end
end
