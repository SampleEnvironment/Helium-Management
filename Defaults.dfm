object DefaultsForm: TDefaultsForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Helium Management :: Default Options'
  ClientHeight = 304
  ClientWidth = 284
  Color = clBtnHighlight
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel2: TBevel
    Left = 0
    Top = 265
    Width = 284
    Height = 1
    Shape = bsFrame
  end
  object PageControl: TPageControl
    Left = 0
    Top = 4
    Width = 284
    Height = 261
    ActivePage = VesselTabSheet
    HotTrack = True
    OwnerDraw = True
    Style = tsFlatButtons
    TabOrder = 0
    OnDrawTab = PageControlDrawTab
    object VesselTabSheet: TTabSheet
      Caption = 'Vessel'
      object HeatLab: TLabel
        Left = 16
        Top = 100
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
        Left = 16
        Top = 58
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
        Left = 16
        Top = 16
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
      object FilltimeLab: TLabel
        Left = 16
        Top = 142
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
      object ZeroLab: TLabel
        Left = 16
        Top = 184
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
      object LongIntLab: TLabel
        Left = 158
        Top = 16
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
      object MaxResLab: TLabel
        Left = 158
        Top = 58
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
      object AdcLab: TLabel
        Left = 158
        Top = 100
        Width = 94
        Height = 11
        Caption = 'ADC Loop (loop count)'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object SpanLab: TLabel
        Left = 158
        Top = 142
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
      object HeatEd: TOraEdit
        Left = 8
        Top = 112
        Width = 121
        Height = 21
        MaxLength = 5
        TabOrder = 4
        OnChange = PropertyOnChange
        InsertMode = imInteger
      end
      object MinResEd: TOraEdit
        Left = 8
        Top = 70
        Width = 121
        Height = 21
        MaxLength = 6
        TabOrder = 2
        OnChange = PropertyOnChange
        InsertMode = imFloat
      end
      object ShortIntEd: TOraEdit
        Left = 8
        Top = 28
        Width = 121
        Height = 21
        MaxLength = 5
        TabOrder = 0
        OnChange = PropertyOnChange
        InsertMode = imInteger
      end
      object FilltimeEd: TOraEdit
        Left = 8
        Top = 154
        Width = 121
        Height = 21
        MaxLength = 3
        TabOrder = 6
        OnChange = PropertyOnChange
        InsertMode = imInteger
      end
      object ZeroEd: TOraEdit
        Left = 8
        Top = 196
        Width = 121
        Height = 21
        MaxLength = 5
        TabOrder = 8
        OnChange = PropertyOnChange
        InsertMode = imInteger
      end
      object LongIntEd: TOraEdit
        Left = 150
        Top = 28
        Width = 121
        Height = 21
        MaxLength = 5
        TabOrder = 1
        OnChange = PropertyOnChange
        InsertMode = imInteger
      end
      object MaxResEd: TOraEdit
        Left = 150
        Top = 70
        Width = 121
        Height = 21
        MaxLength = 6
        TabOrder = 3
        OnChange = PropertyOnChange
        InsertMode = imFloat
      end
      object AdcEd: TOraEdit
        Left = 150
        Top = 112
        Width = 121
        Height = 21
        MaxLength = 3
        TabOrder = 5
        OnChange = PropertyOnChange
        InsertMode = imInteger
      end
      object SpanEd: TOraEdit
        Left = 150
        Top = 154
        Width = 121
        Height = 21
        MaxLength = 5
        TabOrder = 7
        OnChange = PropertyOnChange
        InsertMode = imInteger
      end
    end
    object LevelmeterTabSheet: TTabSheet
      Caption = 'Levelmeter'
      object cVoltAlarmLab: TLabel
        Left = 16
        Top = 58
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
      object cVoltMinLab: TLabel
        Left = 16
        Top = 16
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
      object cVoltMaxLab: TLabel
        Left = 158
        Top = 16
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
      object VoltMinEd: TOraEdit
        Left = 8
        Top = 70
        Width = 121
        Height = 21
        MaxLength = 6
        TabOrder = 0
        OnChange = PropertyOnChange
        InsertMode = imFloat
      end
      object VoltAlarmEd: TOraEdit
        Left = 8
        Top = 28
        Width = 121
        Height = 21
        MaxLength = 5
        TabOrder = 2
        OnChange = PropertyOnChange
        InsertMode = imInteger
      end
      object VoltMaxEd: TOraEdit
        Left = 150
        Top = 28
        Width = 121
        Height = 21
        MaxLength = 6
        TabOrder = 1
        OnChange = PropertyOnChange
        InsertMode = imFloat
      end
    end
  end
  object cOkBtn: TButton
    Left = 24
    Top = 272
    Width = 75
    Height = 25
    Caption = 'OK'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 1
    OnClick = cOkBtnClick
  end
  object CancelBtn: TButton
    Left = 105
    Top = 272
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 2
    OnClick = CancelBtnClick
  end
  object ApplyBtn: TButton
    Left = 186
    Top = 272
    Width = 75
    Height = 25
    Caption = 'Apply'
    TabOrder = 3
    OnClick = ApplyBtnClick
  end
end
