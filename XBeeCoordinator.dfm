object CoordinatorForm: TCoordinatorForm
  Left = 0
  Top = 0
  BorderStyle = bsSingle
  Caption = 'Helium Management :: Coordinator'
  ClientHeight = 358
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
    Top = 88
    Width = 90
    Height = 11
    Caption = 'Coordinator (netnode)'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object IpLab: TLabel
    Left = 24
    Top = 130
    Width = 46
    Height = 11
    Caption = 'IP Address'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object AddressLab: TLabel
    Left = 24
    Top = 172
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
  object PositionLab: TLabel
    Left = 24
    Top = 214
    Width = 32
    Height = 11
    Caption = 'Position'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object CommentLab: TLabel
    Left = 240
    Top = 130
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
  object PortLab: TLabel
    Left = 240
    Top = 88
    Width = 43
    Height = 11
    Caption = 'COM Port'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object cooLab: TLabel
    Left = 24
    Top = 18
    Width = 90
    Height = 11
    Caption = 'Coordinator (netnode)'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object HintLab: TLabel
    Left = 24
    Top = 280
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
  object cooCoBox: TComboBox
    Left = 16
    Top = 30
    Width = 185
    Height = 21
    Style = csDropDownList
    ItemHeight = 0
    TabOrder = 0
    OnChange = cooCoBoxChange
  end
  object cooPanel: TPanel
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
  object IpEd: TOraEdit
    Left = 16
    Top = 142
    Width = 185
    Height = 21
    MaxLength = 20
    TabOrder = 4
    InsertMode = imString
  end
  object AddressEd: TOraEdit
    Left = 16
    Top = 184
    Width = 185
    Height = 21
    MaxLength = 20
    TabOrder = 5
    InsertMode = imString
  end
  object PositionEd: TOraEdit
    Left = 16
    Top = 226
    Width = 185
    Height = 21
    MaxLength = 50
    TabOrder = 6
    InsertMode = imString
  end
  object CommentRiEd: TRichEdit
    Left = 232
    Top = 142
    Width = 185
    Height = 105
    Lines.Strings = (
      'RichEdit1')
    TabOrder = 7
  end
  object PortCoBox: TComboBox
    Left = 232
    Top = 100
    Width = 185
    Height = 21
    Style = csDropDownList
    ItemHeight = 0
    TabOrder = 3
    OnChange = PortCoBoxChange
  end
  object SaveBtn: TButton
    Left = 334
    Top = 258
    Width = 75
    Height = 25
    Caption = 'Save'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 8
    OnClick = SaveBtnClick
  end
  object DeleteBtn: TButton
    Left = 334
    Top = 289
    Width = 75
    Height = 25
    Caption = 'Delete'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 9
    OnClick = DeleteBtnClick
  end
  object CancelBtn: TButton
    Left = 334
    Top = 327
    Width = 75
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 10
  end
end
