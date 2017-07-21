object ModuleForm: TModuleForm
  Left = 0
  Top = 0
  BorderStyle = bsSingle
  Caption = 'Helium Management :: Levelmeter'
  ClientHeight = 399
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
  object NameLab: TLabel
    Left = 24
    Top = 18
    Width = 78
    Height = 11
    Caption = 'Levelmeter (name)'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object LastMsgLab: TLabel
    Left = 24
    Top = 60
    Width = 82
    Height = 11
    Caption = 'Last message (date)'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object eNameLab: TLabel
    Left = 24
    Top = 130
    Width = 25
    Height = 11
    Caption = 'Name'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object eVoltMinLab: TLabel
    Left = 24
    Top = 172
    Width = 108
    Height = 11
    Caption = 'Min Battery voltage (volt)'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object eVoltMaxLab: TLabel
    Left = 24
    Top = 214
    Width = 110
    Height = 11
    Caption = 'Max Battery voltage (volt)'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object eVoltAlarmLab: TLabel
    Left = 24
    Top = 256
    Width = 118
    Height = 11
    Caption = 'Critical Battery capacity (%)'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object eCommentLab: TLabel
    Left = 240
    Top = 172
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
  object eAddressLab: TLabel
    Left = 240
    Top = 130
    Width = 63
    Height = 11
    Caption = 'Address (XBee)'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object ActiveLab: TLabel
    Left = 240
    Top = 35
    Width = 41
    Height = 11
    Caption = 'ActiveLab'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object HintLab: TLabel
    Left = 24
    Top = 343
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
  object Label1: TLabel
    Left = 24
    Top = 297
    Width = 66
    Height = 11
    Caption = 'Critical Voltage:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object modCoBox: TComboBox
    Left = 16
    Top = 30
    Width = 185
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 0
    OnChange = modCoBoxChange
  end
  object LastMsgEd: TEdit
    Left = 16
    Top = 72
    Width = 185
    Height = 21
    Enabled = False
    TabOrder = 1
  end
  object ePanel: TPanel
    Left = 16
    Top = 113
    Width = 401
    Height = 2
    TabOrder = 2
  end
  object NameEd: TOraEdit
    Left = 16
    Top = 142
    Width = 185
    Height = 21
    MaxLength = 50
    TabOrder = 3
    InsertMode = imString
  end
  object VoltMinEd: TOraEdit
    Left = 16
    Top = 184
    Width = 185
    Height = 21
    MaxLength = 5
    TabOrder = 5
    InsertMode = imFloat
  end
  object VoltMaxEd: TOraEdit
    Left = 16
    Top = 226
    Width = 185
    Height = 21
    MaxLength = 5
    TabOrder = 6
    InsertMode = imFloat
  end
  object VoltAlarmEd: TOraEdit
    Left = 16
    Top = 268
    Width = 185
    Height = 21
    MaxLength = 3
    TabOrder = 7
    InsertMode = imInteger
  end
  object CommentRiEd: TRichEdit
    Left = 232
    Top = 184
    Width = 185
    Height = 105
    Lines.Strings = (
      'RichEdit1')
    TabOrder = 8
  end
  object AddressEd: TOraEdit
    Left = 232
    Top = 142
    Width = 185
    Height = 21
    MaxLength = 20
    TabOrder = 4
    InsertMode = imString
  end
  object SaveBtn: TButton
    Left = 334
    Top = 300
    Width = 75
    Height = 25
    Caption = 'Save'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 9
    OnClick = SaveBtnClick
  end
  object DeleteBtn: TButton
    Left = 334
    Top = 331
    Width = 75
    Height = 25
    Caption = 'Delete'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 10
    OnClick = DeleteBtnClick
  end
  object CancelBtn: TButton
    Left = 334
    Top = 369
    Width = 75
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 11
  end
  object VoltCritEd: TEdit
    Left = 16
    Top = 309
    Width = 185
    Height = 21
    Ctl3D = True
    Enabled = False
    ParentCtl3D = False
    TabOrder = 12
  end
end
