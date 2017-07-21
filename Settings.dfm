object SettingsForm: TSettingsForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Helium Management :: Control Panel'
  ClientHeight = 220
  ClientWidth = 284
  Color = clBtnHighlight
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCloseQuery = FormCloseQuery
  DesignSize = (
    284
    220)
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel2: TBevel
    Left = 0
    Top = 181
    Width = 284
    Height = 1
    Shape = bsFrame
  end
  object cOkBtn: TButton
    Left = 24
    Top = 188
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
    Top = 188
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 2
    OnClick = CancelBtnClick
  end
  object ApplyBtn: TButton
    Left = 186
    Top = 188
    Width = 75
    Height = 25
    Caption = 'Apply'
    TabOrder = 3
    OnClick = ApplyBtnClick
  end
  object PageControl: TPageControl
    Left = 0
    Top = 4
    Width = 284
    Height = 177
    ActivePage = DatabaseTabSheet
    HotTrack = True
    OwnerDraw = True
    Style = tsFlatButtons
    TabOrder = 0
    OnDrawTab = PageControlDrawTab
    object PositionsTabSheet: TTabSheet
      Caption = 'Positions'
      object newLab: TLabel
        Left = 158
        Top = 42
        Width = 55
        Height = 11
        Caption = 'New Position'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object posLab: TLabel
        Left = 16
        Top = 16
        Width = 46
        Height = 11
        Caption = 'Position list'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object AddSpeedBtn: TSpeedButton
        Left = 231
        Top = 49
        Width = 25
        Height = 25
        Hint = 'Add'
        Glyph.Data = {
          E6040000424DE604000000000000360000002800000014000000140000000100
          180000000000B0040000C40E0000C40E00000000000000000000C0C0C0C0C0C0
          C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0008000008000008000008000C0C0
          C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
          C0C0C0C0C0008000008000008000008000008000008000008000008000008000
          008000C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C00080
          0000800000800000800000800000800000800000800000800000800000800000
          8000C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0008000008000008000
          008000008000008000FFFFFFFFFFFF0080000080000080000080000080000080
          00C0C0C0C0C0C0C0C0C0C0C0C0C0C0C000800000800000800000800000800000
          8000008000FFFFFFFFFFFF008000008000008000008000008000008000008000
          C0C0C0C0C0C0C0C0C00080000080000080000080000080000080000080000080
          00FFFFFFFFFFFF008000008000008000008000008000008000008000008000C0
          C0C0C0C0C0008000008000008000008000008000008000008000008000FFFFFF
          FFFFFF008000008000008000008000008000008000008000008000C0C0C0C0C0
          C0008000008000008000008000008000008000008000008000FFFFFFFFFFFF00
          8000008000008000008000008000008000008000008000C0C0C0008000008000
          008000008000008000008000008000008000008000FFFFFFFFFFFF0080000080
          00008000008000008000008000008000008000008000008000008000008000FF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFF008000008000008000008000008000008000FFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFF008000008000008000008000008000008000008000008000008000
          008000008000008000FFFFFFFFFFFF0080000080000080000080000080000080
          00008000008000008000C0C0C000800000800000800000800000800000800000
          8000008000FFFFFFFFFFFF008000008000008000008000008000008000008000
          008000C0C0C0C0C0C00080000080000080000080000080000080000080000080
          00FFFFFFFFFFFF008000008000008000008000008000008000008000008000C0
          C0C0C0C0C0008000008000008000008000008000008000008000008000FFFFFF
          FFFFFF008000008000008000008000008000008000008000008000C0C0C0C0C0
          C0C0C0C0008000008000008000008000008000008000008000FFFFFFFFFFFF00
          8000008000008000008000008000008000008000C0C0C0C0C0C0C0C0C0C0C0C0
          C0C0C0008000008000008000008000008000008000FFFFFFFFFFFF0080000080
          00008000008000008000008000C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
          C0C0008000008000008000008000008000008000008000008000008000008000
          008000008000C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
          C0008000008000008000008000008000008000008000008000008000008000C0
          C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
          C0C0C0C0C0C0008000008000008000008000C0C0C0C0C0C0C0C0C0C0C0C0C0C0
          C0C0C0C0C0C0C0C0C0C0}
        ParentShowHint = False
        ShowHint = True
        OnClick = AddSpeedBtnClick
      end
      object Bevel1: TBevel
        Left = 140
        Top = 80
        Width = 130
        Height = 1
        Shape = bsFrame
      end
      object DelSpeedBtn: TSpeedButton
        Left = 231
        Top = 87
        Width = 25
        Height = 25
        Hint = 'Delete'
        Glyph.Data = {
          E6040000424DE604000000000000360000002800000014000000140000000100
          180000000000B0040000C40E0000C40E00000000000000000000C0C0C0C0C0C0
          C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C00000FF0000FF0000FF0000FFC0C0
          C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
          C0C0C0C0C00000FF0000FF0000FF0000FF0000FF0000FF0000FF0000FF0000FF
          0000FFC0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C00000
          FF0000FF0000FF0000FF0000FF0000FF0000FF0000FF0000FF0000FF0000FF00
          00FFC0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C00000FF0000FF
          0000FF0000FF0000FF0000FF0000FF0000FF0000FF0000FF0000FF0000FFC0C0
          C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C00000FF0000FF0000FF0000FF0000FF00
          00FF0000FF0000FF0000FF0000FF0000FF0000FF0000FF0000FF0000FF0000FF
          C0C0C0C0C0C0C0C0C00000FF0000FF0000FF0000FFFFFFFFFFFFFF0000FF0000
          FF0000FF0000FF0000FF0000FFFFFFFFFFFFFF0000FF0000FF0000FF0000FFC0
          C0C0C0C0C00000FF0000FF0000FF0000FFFFFFFFFFFFFFFFFFFF0000FF0000FF
          0000FF0000FFFFFFFFFFFFFFFFFFFF0000FF0000FF0000FF0000FFC0C0C0C0C0
          C00000FF0000FF0000FF0000FF0000FFFFFFFFFFFFFFFFFFFF0000FF0000FFFF
          FFFFFFFFFFFFFFFF0000FF0000FF0000FF0000FF0000FFC0C0C00000FF0000FF
          0000FF0000FF0000FF0000FF0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FF0000FF0000FF0000FF0000FF0000FF0000FF0000FF0000FF0000FF0000FF00
          00FF0000FF0000FF0000FF0000FFFFFFFFFFFFFFFFFFFFFFFFFF0000FF0000FF
          0000FF0000FF0000FF0000FF0000FF0000FF0000FF0000FF0000FF0000FF0000
          FF0000FF0000FF0000FFFFFFFFFFFFFFFFFFFFFFFFFF0000FF0000FF0000FF00
          00FF0000FF0000FF0000FF0000FF0000FF0000FF0000FF0000FF0000FF0000FF
          0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000FF0000FF0000FF0000
          FF0000FF0000FF0000FFC0C0C00000FF0000FF0000FF0000FF0000FFFFFFFFFF
          FFFFFFFFFF0000FF0000FFFFFFFFFFFFFFFFFFFF0000FF0000FF0000FF0000FF
          0000FFC0C0C0C0C0C00000FF0000FF0000FF0000FFFFFFFFFFFFFFFFFFFF0000
          FF0000FF0000FF0000FFFFFFFFFFFFFFFFFFFF0000FF0000FF0000FF0000FFC0
          C0C0C0C0C00000FF0000FF0000FF0000FFFFFFFFFFFFFF0000FF0000FF0000FF
          0000FF0000FF0000FFFFFFFFFFFFFF0000FF0000FF0000FF0000FFC0C0C0C0C0
          C0C0C0C00000FF0000FF0000FF0000FF0000FF0000FF0000FF0000FF0000FF00
          00FF0000FF0000FF0000FF0000FF0000FF0000FFC0C0C0C0C0C0C0C0C0C0C0C0
          C0C0C0C0C0C00000FF0000FF0000FF0000FF0000FF0000FF0000FF0000FF0000
          FF0000FF0000FF0000FFC0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
          C0C00000FF0000FF0000FF0000FF0000FF0000FF0000FF0000FF0000FF0000FF
          0000FF0000FFC0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
          C00000FF0000FF0000FF0000FF0000FF0000FF0000FF0000FF0000FF0000FFC0
          C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
          C0C0C0C0C0C00000FF0000FF0000FF0000FFC0C0C0C0C0C0C0C0C0C0C0C0C0C0
          C0C0C0C0C0C0C0C0C0C0}
        ParentShowHint = False
        ShowHint = True
        OnClick = DelSpeedBtnClick
      end
      object Label1: TLabel
        Left = 150
        Top = 95
        Width = 70
        Height = 11
        Caption = 'Selected Position'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object posLiBox: TListBox
        Left = 8
        Top = 28
        Width = 121
        Height = 105
        BevelInner = bvNone
        BevelOuter = bvNone
        Ctl3D = False
        ItemHeight = 13
        ParentCtl3D = False
        TabOrder = 0
        OnKeyDown = posLiBoxKeyDown
      end
      object newEd: TEdit
        Left = 150
        Top = 55
        Width = 75
        Height = 19
        BevelInner = bvNone
        BevelOuter = bvNone
        Ctl3D = False
        MaxLength = 3
        ParentCtl3D = False
        TabOrder = 1
        OnKeyDown = newEdKeyDown
      end
    end
    object LevelmeterTabSheet: TTabSheet
      Caption = 'Levelmeter'
      object sleepLab: TLabel
        Left = 16
        Top = 58
        Width = 66
        Height = 11
        Caption = 'XBee sleep time'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object passwLab: TLabel
        Left = 16
        Top = 100
        Width = 40
        Height = 11
        Caption = 'Password'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object awakeLab: TLabel
        Left = 16
        Top = 16
        Width = 73
        Height = 11
        Caption = 'XBee awake time'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object sleepCoBox: TComboBox
        Left = 8
        Top = 70
        Width = 121
        Height = 21
        BevelInner = bvNone
        BevelOuter = bvNone
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 1
        OnChange = PropertyOnChange
      end
      object passwCoBox: TComboBox
        Left = 8
        Top = 112
        Width = 121
        Height = 21
        BevelInner = bvNone
        BevelOuter = bvNone
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 2
        OnChange = PropertyOnChange
      end
      object awakeCoBox: TComboBox
        Left = 8
        Top = 28
        Width = 121
        Height = 21
        BevelInner = bvNone
        BevelOuter = bvNone
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 0
        OnChange = PropertyOnChange
      end
    end
    object DatabaseTabSheet: TTabSheet
      Caption = 'Database'
      object serverLab: TLabel
        Left = 16
        Top = 64
        Width = 50
        Height = 11
        Caption = 'data source:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object userLab: TLabel
        Left = 16
        Top = 106
        Width = 42
        Height = 11
        Caption = 'Username'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object pwLab: TLabel
        Left = 158
        Top = 106
        Width = 40
        Height = 11
        Caption = 'Password'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object deleteLab: TLabel
        Left = 158
        Top = 64
        Width = 90
        Height = 11
        Caption = 'Delete measurements'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object DatabaseTypeLab: TLabel
        Left = 18
        Top = 15
        Width = 85
        Height = 11
        Caption = 'select database type'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object dbuserEd: TEdit
        Left = 8
        Top = 118
        Width = 120
        Height = 21
        TabOrder = 0
        OnChange = PropertyOnChange
      end
      object dbpwEd: TEdit
        Left = 150
        Top = 118
        Width = 120
        Height = 21
        PasswordChar = '*'
        TabOrder = 1
        OnChange = PropertyOnChange
      end
      object deleteCoBox: TComboBox
        Left = 150
        Top = 76
        Width = 121
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 2
        OnChange = PropertyOnChange
      end
      object selectdbtypeCoBox: TComboBox
        Left = 8
        Top = 27
        Width = 262
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 3
        OnChange = PropertyOnChange
      end
    end
    object AdvancedTabSheet: TTabSheet
      Caption = 'Advanced'
      object RetryAttemptsLab: TLabel
        Left = 72
        Top = 61
        Width = 133
        Height = 13
        Caption = 'Coordinator Retry attempts'
      end
      object debugChBox: TCheckBox
        Left = 8
        Top = 24
        Width = 97
        Height = 17
        Caption = 'Debug mode'
        TabOrder = 0
        OnClick = debugChBoxClick
      end
      object RetryAttemptsSpinEdit: TSpinEdit
        Left = 3
        Top = 58
        Width = 48
        Height = 22
        Ctl3D = True
        MaxValue = 10
        MinValue = 0
        ParentCtl3D = False
        TabOrder = 1
        Value = 0
        OnChange = PropertyOnChange
      end
    end
  end
  object PanelDS: TPanel
    Left = 12
    Top = 107
    Width = 120
    Height = 21
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvNone
    BorderStyle = bsSingle
    Ctl3D = False
    ParentColor = True
    ParentCtl3D = False
    TabOrder = 4
    DesignSize = (
      118
      19)
    object BtnEditDS: TFlatGlyphBtn
      Left = 100
      Top = 1
      Width = 17
      Height = 17
      Anchors = [akTop, akRight]
      AntiliasingDegree = 1.000000000000000000
      BorderRadius = 15
      Color = 12632256
      Glyph.Data = {
        5E170000424D5E17000000000000360000002800000033000000260000000100
        1800000000002817000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000303030000000000000000000000000303030505050000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000606060505050000000606060202020000000101010101010000000000
        0000000000000000000000000000000000000000000000000000000000000000
        00000F0F0F020202000000000000000000000000030303020202020202020202
        0202020202020202020202020202020202020202020202020202020202020202
        0202020202020202020202020202020202020202020202020202020202020200
        00000000000000000505050000000C0C0C020202000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        000202020101011313130E0E0E00000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000606060909
        090A0A0A04040400000007070706060600000000000000000000000000000000
        00000000000000000000000000000000000000000000000C0C0C000000303030
        1A1A1A2626262626262525252626262B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B
        2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B
        2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2B2424242525252C2C2C
        2F2F2F2020200000000303030101010000000000000000000000000000000000
        000000000000000000000000000000000000000303030B0B0BE3E3E3FFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFE
        FEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFE
        FEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFFFFFFFCFCFCF9F9F9F9F9F9D4
        D4D40E0E0E0F0F0F000000000000000000000000000000000000000000000000
        000000000000000000000000000000000000000000ACACACF8F8F8F3F3F3EBEB
        EBFFFFFFFFFFFFFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFE
        FEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFE
        FEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF1919
        1900000001010100000000000000000000000000000000000000000000000000
        00000000000000000000000101011919195B5B5BFFFFFFFFFFFFFFFFFFFFFFFF
        FCFCFCFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFE
        FEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFE
        FEFEFEFEFEFEFEFEFEFEFEFFFFFFFFFFFFFAFAFAF8F8F8FEFEFE535353000000
        0808080000000000000000000000000000000000000000000000000000000000
        000000000000000606064C4C4C232323FCFCFCFFFFFFF1F1F1FEFEFEFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFF9F9F9FFFFFFFFFFFFF6F6F6FFFFFF99999900000003030300
        0000000000000000000000000000000000000000000000000000000000000000
        000000000000989898000000D4D4D4FFFFFFF8F8F8FFFFFFFCFCFCFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFCFCFCFFFFFFFFFFFFF1F1F1FFFFFFFFFFFF0000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000005
        0505C7C7C71010109E9E9EFBFBFBFFFFFFFFFFFFFAFAFAFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFDFDFDFBFBFBFFFFFFFFFFFF131313070707000000000000000000
        000000000000000000000000000000000000000000000000000000060606E5E5
        E5272727585858FBFBFBFDFDFDFFFFFFF8F8F8FFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFF7F7F7FFFFFFFFFFFFF9F9F944444409090900000000000000000000000000
        0000000000000000000000000000000000000000000000000000E0E0E04E4E4E
        262626FFFFFFF5F5F5F7F7F7FCFCFCFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF5F5F5
        FFFFFFFFFFFFFCFCFC8080800303030000000000000000000000000000000000
        00000000000000000000000000000000000000000000D9D9D9919191060606F9
        F9F9F7F7F7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFEFEFFFFFFF9F9F9FFFFFFFA
        FAFAFFFFFFB7B7B7010101000000000000000000000000000000000000000000
        000000000000000000000000000000000000D5D5D5D5D5D5000000ACACACFBFB
        FBFFFFFFF7F7F7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFDFDFDFFFFFFFFFFFFFFFFFFF8F8F8FFFF
        FFE1E1E117171700000000000000000000000000000000000000000000000000
        0000000000000000000000000000C7C7C7FDFDFD141414585858FDFDFDFEFEFE
        F9F9F9FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFBFBFBFFFFFFFFFFFFF8F8F8FFFFFFFAFAFA
        4242420000000000000000000000000000000000000000000000000000000000
        000000000000000D0D0DB8B8B8FFFFFF414141313131FFFFFFF0F0F0FFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFF5F5F5FFFFFFFFFFFFFAFAFAFFFFFFFFFFFF65656500
        0000000000000000000000000000000000000000000000000000000000000000
        000000000000CBCBCBFDFDFD8B8B8B000000DFDFDFFFFFFFFAFAFAFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFF9F9F9FFFFFFFFFFFFFFFFFFFFFFFFECECECFDFDFDB5B5B50303030606
        0600000000000000000000000000000000000000000000000000000000000000
        0000CDCDCDFFFFFFEAEAEA010101B0B0B0F4F4F4F8F8F8FFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFBFBFBFFFFFFFFFFFFFAFAFAFFFFFFEEEEEE0000000505050A0A0A
        020202000000000000000000000000000000000000000000000000070707C6C6
        C6F2F2F2FFFFFF050505626262FFFFFFFFFFFFFEFEFEFEFEFEFEFEFEFEFEFEFE
        FEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFE
        FEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFFFFFFF9F9
        F9F9F9F9FFFFFFF7F7F7FCFCFCFFFFFFFFFFFF24242400000004040400000000
        0000000000000000000000000000000000000000000000000000C4C4C4FFFFFF
        FFFFFF4C4C4C202020FFFFFFF9F9F9FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFDFDFDFFFFFF
        FFFFFFFAFAFAFFFFFFF7F7F7FCFCFC7070700000000000000101010000000000
        00000000000000000000000000000000000000080808CCCCCCFFFFFFEEEEEE83
        8383000000DBDBDBF8F8F8FCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFC
        FCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFC
        FCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFAFAFAFFFFFFF8F8F8FF
        FFFFFFFFFFFFFFFFFFFFFF8484840F0F0F010101050505000000000000000000
        000000000000000000000000000000000000C4C4C4F3F3F3FFFFFFC8C8C80202
        021313130F0F0F14141414141414141414141414141414141414141414141414
        1414141414141414141414141414141414141414141414141414141414141414
        1414141414141414141414141414141717171414141414140B0B0B2121210B0B
        0B12121217171734343406060609090900000000000000000000000000000000
        0000000000000000000000090909C9C9C9F1F1F1FFFFFFFFFFFFB2B2B2777777
        7474747979797979797979797979797979797979797979797979797979797979
        7979797979797979797979797979797979797979797979797979797979797979
        79797979797979797979797C7C7C7777777A7A7A7575758080801313130C0C0C
        0404040000000000000101010000000000000000000000000000000000000000
        00000000000000000000CDCDCDFFFFFFE5E5E5F7F7F7FFFFFFF4F4F4FFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFAFAFAFFFFFFFFFFFFF9F9F93434340D0D0D00000000
        0000060606000000050505000000000000000000000000000000000000000000
        000000000000E1E1E1F3F3F3FFFFFFFCFCFCFFFFFFF7F7F7FFFFFFFEFEFEF9F9
        F9FFFFFFFDFDFDFCFCFCF2F2F2FFFFFFFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFD
        FDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFD
        FDFDFDFDFDFDFBFBFBFFFFFFF1F1F1F7F7F73333330000000707070000000000
        0000000000000000000000000000000000000000000000000000000000000000
        00004E4E4E636363949494FFFFFFFCFCFCF9F9F9F4F4F4FFFFFFFFFFFFF7F7F7
        FFFFFFFBFBFBFFFFFFFFFFFF7E7E7E6161616161616161616161616161616161
        6161616161616161616161616161616161616161616161616161616161616160
        60606363636464645C5C5C767676090909000000020202000000000000000000
        0000000000000000000000000000000000000000000000000000000707070000
        00000000141414C4C4C4FFFFFFFFFFFFF8F8F8FEFEFEFEFEFEF5F5F5FFFFFFFF
        FFFFFFFFFFA7A7A7040404000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000101
        010000000000000F0F0F00000003030300000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000010101000000
        000000595959EAEAEAFFFFFFF9F9F9FFFFFFFFFFFFFFFFFFF4F4F4FDFDFDE1E1
        E130303005050500000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000404040A0A0A0000000000000000000000000000000000000000
        0000000000000000000000000000000000000008080803030301010103030300
        00003030301E1E1E3232322323232828283131312929293E3E3E424242000000
        1313130202020202020202020202020202020202020202020202020202020202
        0202020202020202020202020202020202020202020200000005050509090900
        0000050505020202000000000000000000000000000000000000000000000000
        0000000000000000000000000000000202020000000606060808080000000707
        0700000000000000000008080800000001010100000000000004040400000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000101010000000303030606060000000000
        0000000002020200000000000000000000000000000000000000000000000000
        00000000000000000000000606060000000A0A0A000000020202080808080808
        0000000000000808080000000707070202020000001E1E1E0000000202020202
        0202020202020202020202020202020202020202020202020202020202020202
        0202020202020202020202070707000000090909000000010101010101000000
        0707070000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000001010102020200000000
        0000040404040404000000111111000000000000050505000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000004040400000009090900000004040404040400000002020200
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000}
      LightDegree = 0.500000000000000000
      MonochromeTransparent = True
      OnClick = BtnEditDSClick
    end
    object EdDataSource: TEdit
      Left = 2
      Top = 2
      Width = 95
      Height = 15
      Anchors = [akLeft, akTop, akRight, akBottom]
      BorderStyle = bsNone
      Enabled = False
      TabOrder = 0
      OnChange = PropertyOnChange
    end
  end
end