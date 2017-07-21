object TextMsgForm: TTextMsgForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Helium Management :: Msg'
  ClientHeight = 142
  ClientWidth = 204
  Color = clBtnHighlight
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object textLab: TLabel
    Left = 16
    Top = 58
    Width = 54
    Height = 11
    Caption = 'message text'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object typeLab: TLabel
    Left = 16
    Top = 16
    Width = 58
    Height = 11
    Caption = 'message type'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object sendBtn: TButton
    Left = 24
    Top = 109
    Width = 75
    Height = 25
    Caption = 'Send'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 2
    OnClick = sendBtnClick
  end
  object typeCoBox: TComboBox
    Left = 8
    Top = 28
    Width = 185
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 0
  end
  object CancelBtn: TButton
    Left = 105
    Top = 109
    Width = 75
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object textEd: TEdit
    Left = 8
    Top = 70
    Width = 185
    Height = 21
    MaxLength = 18
    TabOrder = 1
  end
end
