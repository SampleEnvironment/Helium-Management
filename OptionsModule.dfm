object ModuleOptionForm: TModuleOptionForm
  Left = 0
  Top = 0
  BorderStyle = bsSingle
  Caption = 'Helium Management :: Levelmeter Options'
  ClientHeight = 351
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
  object dNameLab: TLabel
    Left = 119
    Top = 56
    Width = 74
    Height = 11
    Caption = 'Levelmeter Name'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object addressLab: TLabel
    Left = 119
    Top = 14
    Width = 33
    Height = 11
    Caption = 'Address'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object curLab: TLabel
    Left = 32
    Top = 112
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
    Top = 112
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
  object cVoltMaxLab: TLabel
    Left = 24
    Top = 148
    Width = 101
    Height = 11
    Caption = 'Max Battery voltage (V)'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object cVoltMinLab: TLabel
    Left = 24
    Top = 190
    Width = 99
    Height = 11
    Caption = 'Min Battery voltage (V)'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object cVoltAlarmLab: TLabel
    Left = 24
    Top = 232
    Width = 76
    Height = 11
    Caption = 'Battery alarm (%)'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object nameEd: TEdit
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
  object addressCoBox: TComboBox
    Left = 111
    Top = 26
    Width = 185
    Height = 21
    ItemHeight = 13
    TabOrder = 0
    Text = 'please select ...'
    OnChange = addressCoBoxChange
  end
  object cVoltMaxEd: TEdit
    Left = 16
    Top = 160
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
  object cVoltMinEd: TEdit
    Left = 16
    Top = 202
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
  object uVoltMaxEd: TEdit
    Left = 207
    Top = 160
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
  object uVoltMinEd: TEdit
    Left = 207
    Top = 202
    Width = 185
    Height = 21
    Enabled = False
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 6
  end
  object VoltMaxChBox: TCheckBox
    Left = 398
    Top = 162
    Width = 19
    Height = 17
    TabOrder = 4
  end
  object VoltMinChBox: TCheckBox
    Left = 398
    Top = 204
    Width = 19
    Height = 17
    TabOrder = 7
  end
  object cVoltAlarmEd: TEdit
    Left = 16
    Top = 244
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
  object uVoltAlarmEd: TEdit
    Left = 207
    Top = 244
    Width = 185
    Height = 21
    Enabled = False
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 9
  end
  object VoltAlarmBox: TCheckBox
    Left = 398
    Top = 246
    Width = 19
    Height = 17
    TabOrder = 10
  end
  object AcceptBtn: TButton
    Left = 236
    Top = 279
    Width = 75
    Height = 25
    Caption = 'Accept'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 11
    OnClick = AcceptBtnClick
  end
  object DiscardBtn: TButton
    Left = 317
    Top = 279
    Width = 75
    Height = 25
    Caption = 'Discard'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 12
    OnClick = DiscardBtnClick
  end
  object CancelBtn: TButton
    Left = 317
    Top = 319
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 13
    OnClick = CancelBtnClick
  end
end
