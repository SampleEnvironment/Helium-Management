object OptionsForm: TOptionsForm
  Left = 0
  Top = 0
  BorderStyle = bsSingle
  Caption = 'Helium Management :: Vessel Options'
  ClientHeight = 671
  ClientWidth = 430
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
  object cShortIntLab: TLabel
    Left = 24
    Top = 220
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
  object cLongIntLab: TLabel
    Left = 24
    Top = 262
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
  object cMaxResLab: TLabel
    Left = 24
    Top = 346
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
  object cMinResLab: TLabel
    Left = 24
    Top = 304
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
  object cHeatLab: TLabel
    Left = 24
    Top = 388
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
  object cAdcLab: TLabel
    Left = 24
    Top = 430
    Width = 125
    Height = 11
    Caption = 'ADC Loop (number of cycles) '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object cFillTimeLab: TLabel
    Left = 24
    Top = 472
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
  object curLab: TLabel
    Left = 32
    Top = 184
    Width = 139
    Height = 19
    Caption = 'Current Settings '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = [fsBold, fsItalic, fsUnderline]
    ParentFont = False
  end
  object newLab: TLabel
    Left = 224
    Top = 184
    Width = 133
    Height = 19
    Caption = 'Settings by User'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = [fsBold, fsItalic, fsUnderline]
    ParentFont = False
  end
  object typeLab: TLabel
    Left = 119
    Top = 56
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
  object tareLab: TLabel
    Left = 119
    Top = 98
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
  object volumeLab: TLabel
    Left = 119
    Top = 140
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
  object dNameLab: TLabel
    Left = 119
    Top = 14
    Width = 63
    Height = 11
    Caption = 'Vessel Number'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object cSpanLab: TLabel
    Left = 24
    Top = 514
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
  object cZeroLab: TLabel
    Left = 24
    Top = 556
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
  object AcceptBtn: TButton
    Left = 236
    Top = 599
    Width = 75
    Height = 25
    Caption = 'Accept'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 31
    OnClick = AcceptBtnClick
  end
  object DiscardBtn: TButton
    Left = 317
    Top = 599
    Width = 75
    Height = 25
    Caption = 'Discard'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 32
    OnClick = DiscardBtnClick
  end
  object CancelBtn: TButton
    Left = 317
    Top = 639
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 33
    OnClick = CancelBtnClick
  end
  object cShortIntEd: TEdit
    Left = 16
    Top = 232
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
  object cLongIntEd: TEdit
    Left = 16
    Top = 274
    Width = 185
    Height = 21
    Enabled = False
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 7
  end
  object cMaxResEd: TEdit
    Left = 16
    Top = 358
    Width = 185
    Height = 21
    Enabled = False
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 13
  end
  object cMinResEd: TEdit
    Left = 16
    Top = 316
    Width = 185
    Height = 21
    Enabled = False
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 10
  end
  object cHeatEd: TEdit
    Left = 16
    Top = 400
    Width = 185
    Height = 21
    Enabled = False
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 16
  end
  object cAdcEd: TEdit
    Left = 16
    Top = 442
    Width = 185
    Height = 21
    Enabled = False
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 19
  end
  object cFillTimeEd: TEdit
    Left = 16
    Top = 484
    Width = 185
    Height = 21
    Enabled = False
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 22
  end
  object uShortIntEd: TEdit
    Left = 207
    Top = 232
    Width = 185
    Height = 21
    Enabled = False
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 5
  end
  object uLongIntEd: TEdit
    Left = 207
    Top = 274
    Width = 185
    Height = 21
    Enabled = False
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 8
  end
  object uMinResEd: TEdit
    Left = 207
    Top = 316
    Width = 185
    Height = 21
    Enabled = False
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 11
  end
  object uMaxResEd: TEdit
    Left = 207
    Top = 358
    Width = 185
    Height = 21
    Enabled = False
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 14
  end
  object uHeatEd: TEdit
    Left = 207
    Top = 400
    Width = 185
    Height = 21
    Enabled = False
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 17
  end
  object uAdcEd: TEdit
    Left = 207
    Top = 442
    Width = 185
    Height = 21
    Enabled = False
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 20
  end
  object uFillTimeEd: TEdit
    Left = 207
    Top = 484
    Width = 185
    Height = 21
    Enabled = False
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 23
  end
  object shortIntChBox: TCheckBox
    Left = 398
    Top = 234
    Width = 19
    Height = 17
    TabOrder = 6
  end
  object longIntChBox: TCheckBox
    Left = 398
    Top = 276
    Width = 19
    Height = 17
    TabOrder = 9
  end
  object minResChBox: TCheckBox
    Left = 398
    Top = 318
    Width = 19
    Height = 17
    TabOrder = 12
  end
  object maxResChBox: TCheckBox
    Left = 398
    Top = 360
    Width = 19
    Height = 17
    TabOrder = 15
  end
  object heatChBox: TCheckBox
    Left = 398
    Top = 402
    Width = 19
    Height = 17
    TabOrder = 18
  end
  object adcChBox: TCheckBox
    Left = 398
    Top = 444
    Width = 19
    Height = 17
    TabOrder = 21
  end
  object filltimeChBox: TCheckBox
    Left = 398
    Top = 486
    Width = 19
    Height = 17
    TabOrder = 24
  end
  object typeEd: TEdit
    Left = 111
    Top = 68
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
  object tareEd: TEdit
    Left = 111
    Top = 110
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
  object volumeEd: TEdit
    Left = 111
    Top = 152
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
  object nameCoBox: TComboBox
    Left = 111
    Top = 26
    Width = 98
    Height = 21
    ItemHeight = 0
    TabOrder = 0
    Text = 'please select ...'
    OnChange = nameCoBoxChange
  end
  object cSpanEd: TEdit
    Left = 16
    Top = 526
    Width = 185
    Height = 21
    Enabled = False
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 25
  end
  object uSpanEd: TEdit
    Left = 207
    Top = 526
    Width = 185
    Height = 21
    Enabled = False
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 26
  end
  object spanChBox: TCheckBox
    Left = 398
    Top = 528
    Width = 19
    Height = 17
    TabOrder = 27
  end
  object cZeroEd: TEdit
    Left = 16
    Top = 568
    Width = 185
    Height = 21
    Enabled = False
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 28
  end
  object uZeroEd: TEdit
    Left = 207
    Top = 568
    Width = 185
    Height = 21
    Enabled = False
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 29
  end
  object zeroChBox: TCheckBox
    Left = 398
    Top = 570
    Width = 19
    Height = 17
    TabOrder = 30
  end
end
