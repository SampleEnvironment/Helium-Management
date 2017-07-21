object IlmDeviceForm: TIlmDeviceForm
  Left = 0
  Top = 0
  BorderStyle = bsSingle
  Caption = 'Helium Management :: Cryostat'
  ClientHeight = 472
  ClientWidth = 444
  Color = clHighlightText
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  DesignSize = (
    444
    472)
  PixelsPerInch = 96
  TextHeight = 13
  object NameLab: TLabel
    Left = 24
    Top = 88
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
  object AddressLab: TLabel
    Left = 24
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
  object CommentLab: TLabel
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
  object ilmLab: TLabel
    Left = 24
    Top = 18
    Width = 36
    Height = 11
    Caption = 'Cryostat'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object HintLab: TLabel
    Left = 24
    Top = 386
    Width = 43
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'HintLab'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    ExplicitTop = 429
  end
  object Ch1SpanLab: TLabel
    Left = 208
    Top = 226
    Width = 21
    Height = 11
    Caption = 'Span'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Ch1ZeroLab: TLabel
    Left = 325
    Top = 226
    Width = 18
    Height = 11
    Caption = 'Zero'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Ch2SpanLab: TLabel
    Left = 208
    Top = 268
    Width = 21
    Height = 11
    Caption = 'Span'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Ch2ZeroLab: TLabel
    Left = 325
    Top = 268
    Width = 18
    Height = 11
    Caption = 'Zero'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Ch3SpanLab: TLabel
    Left = 208
    Top = 310
    Width = 21
    Height = 11
    Caption = 'Span'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Ch3ZeroLab: TLabel
    Left = 325
    Top = 310
    Width = 18
    Height = 11
    Caption = 'Zero'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Ch1GasLab: TLabel
    Left = 117
    Top = 226
    Width = 16
    Height = 11
    Caption = 'Gas'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Ch2GasLab: TLabel
    Left = 117
    Top = 268
    Width = 16
    Height = 11
    Caption = 'Gas'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Ch3GasLab: TLabel
    Left = 117
    Top = 310
    Width = 16
    Height = 11
    Caption = 'Gas'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Label1: TLabel
    Left = 24
    Top = 173
    Width = 39
    Height = 11
    Caption = 'Category'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Label2: TLabel
    Left = 240
    Top = 173
    Width = 60
    Height = 11
    Caption = 'new Category'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object ilmCoBox: TComboBox
    Left = 16
    Top = 30
    Width = 185
    Height = 21
    Style = csDropDownList
    ItemHeight = 0
    TabOrder = 0
    OnChange = ilmCoBoxChange
  end
  object devPanel: TPanel
    Left = 16
    Top = 71
    Width = 401
    Height = 2
    TabOrder = 1
  end
  object NameEd: TOraEdit
    Left = 16
    Top = 100
    Width = 185
    Height = 21
    MaxLength = 50
    TabOrder = 2
    InsertMode = imString
  end
  object AddressEd: TOraEdit
    Left = 16
    Top = 142
    Width = 185
    Height = 21
    MaxLength = 20
    TabOrder = 3
    InsertMode = imString
  end
  object CommentRiEd: TRichEdit
    Left = 232
    Top = 100
    Width = 185
    Height = 63
    Lines.Strings = (
      'RichEdit1')
    TabOrder = 4
  end
  object SaveBtn: TButton
    Left = 334
    Top = 364
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
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
  object DeleteBtn: TButton
    Left = 334
    Top = 395
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
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
  object CancelBtn: TButton
    Left = 334
    Top = 433
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 19
  end
  object Ch1ZeroEd: TOraEdit
    Left = 317
    Top = 238
    Width = 100
    Height = 21
    MaxLength = 20
    TabOrder = 8
    InsertMode = imFloat
  end
  object Ch1ChkBox: TCheckBox
    Left = 16
    Top = 241
    Width = 71
    Height = 17
    Caption = 'Channel 1'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 5
    OnClick = Ch1ChkBoxClick
  end
  object Ch1SpanEd: TOraEdit
    Left = 200
    Top = 238
    Width = 100
    Height = 21
    MaxLength = 20
    TabOrder = 7
    InsertMode = imFloat
  end
  object Ch2ZeroEd: TOraEdit
    Left = 317
    Top = 280
    Width = 100
    Height = 21
    MaxLength = 20
    TabOrder = 12
    InsertMode = imFloat
  end
  object Ch2ChkBox: TCheckBox
    Left = 16
    Top = 283
    Width = 71
    Height = 17
    Caption = 'Channel 2'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 9
    OnClick = Ch2ChkBoxClick
  end
  object Ch2SpanEd: TOraEdit
    Left = 200
    Top = 280
    Width = 100
    Height = 21
    MaxLength = 20
    TabOrder = 11
    InsertMode = imFloat
  end
  object Ch3ZeroEd: TOraEdit
    Left = 317
    Top = 322
    Width = 100
    Height = 21
    MaxLength = 20
    TabOrder = 16
    InsertMode = imFloat
  end
  object Ch3SpanEd: TOraEdit
    Left = 200
    Top = 322
    Width = 100
    Height = 21
    MaxLength = 20
    TabOrder = 15
    InsertMode = imFloat
  end
  object Ch3ChkBox: TCheckBox
    Left = 16
    Top = 325
    Width = 71
    Height = 17
    Caption = 'Channel 3'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 13
    OnClick = Ch3ChkBoxClick
  end
  object Ch1GasCoBox: TComboBox
    Left = 109
    Top = 238
    Width = 73
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 6
    Items.Strings = (
      'He'
      'N')
  end
  object Ch2GasCoBox: TComboBox
    Left = 109
    Top = 280
    Width = 73
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 10
    Items.Strings = (
      'He'
      'N')
  end
  object Ch3GasCoBox: TComboBox
    Left = 109
    Top = 322
    Width = 73
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 14
    Items.Strings = (
      'He'
      'N')
  end
  object CBCategory: TComboBox
    Left = 16
    Top = 185
    Width = 185
    Height = 21
    ItemHeight = 0
    TabOrder = 20
    OnChange = CBCategoryChange
  end
  object EdNewCatName: TEdit
    Left = 232
    Top = 185
    Width = 185
    Height = 21
    Enabled = False
    TabOrder = 21
  end
end
