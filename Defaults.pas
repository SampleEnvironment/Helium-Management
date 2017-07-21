unit Defaults;

interface

uses
  Windows, SysUtils, Classes, Controls, Forms, StdCtrls, Dialogs, Registry,
  ComCtrls, ExtCtrls, Graphics, DatabaseEdit;

type
  TDefaultsForm = class(TForm)
    PageControl: TPageControl;
    VesselTabSheet: TTabSheet;
    LevelmeterTabSheet: TTabSheet;
    HeatLab: TLabel;
    MinResLab: TLabel;
    ShortIntLab: TLabel;
    FilltimeLab: TLabel;
    HeatEd: TOraEdit;
    MinResEd: TOraEdit;
    ShortIntEd: TOraEdit;
    FilltimeEd: TOraEdit;
    ZeroEd: TOraEdit;
    ZeroLab: TLabel;
    LongIntLab: TLabel;
    LongIntEd: TOraEdit;
    MaxResLab: TLabel;
    MaxResEd: TOraEdit;
    AdcLab: TLabel;
    AdcEd: TOraEdit;
    SpanLab: TLabel;
    SpanEd: TOraEdit;
    cVoltAlarmLab: TLabel;
    cVoltMinLab: TLabel;
    VoltMinEd: TOraEdit;
    VoltAlarmEd: TOraEdit;
    cVoltMaxLab: TLabel;
    VoltMaxEd: TOraEdit;
    Bevel2: TBevel;
    cOkBtn: TButton;
    CancelBtn: TButton;
    ApplyBtn: TButton;
    procedure PageControlDrawTab(Control: TCustomTabControl; TabIndex: Integer;
      const Rect: TRect; Active: Boolean);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure PropertyOnChange(Sender: TObject);
    procedure cOkBtnClick(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure ApplyBtnClick(Sender: TObject);
  private
    procedure Init;
    procedure ApplyValues;
    procedure SaveValues;
  public
    constructor CreateWithProperties(AOwner: TComponent);
    destructor Destroy; override;
  end;

implementation

{$R *.dfm}
    
constructor TDefaultsForm.CreateWithProperties(AOwner: TComponent);
begin
  create(AOwner);

  Init;
  ApplyBtn.Enabled := false;
end;

destructor TDefaultsForm.Destroy;
begin

  inherited Destroy;
end;

procedure TDefaultsForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := not ApplyBtn.Enabled;

  if not CanClose then
    CanClose := (MessageDlg('If you cancel your changes will get lost.' + #13
                            + 'Do you want to cancel anyway?',
                            mtConfirmation, [mbYes,mbNo], 0) = mrYes);
end;

procedure TDefaultsForm.PageControlDrawTab(Control: TCustomTabControl;
  TabIndex: Integer; const Rect: TRect; Active: Boolean);
var
  LCaptionX: Integer;
  LCaptionY: Integer;
  LTabCaption: string;
begin
  with Control.Canvas do
  begin
    Font.Color  := clWindowText;
    Brush.Style := bsSolid;

    if (TabIndex = TPageControl(Control).TabIndex) then
      Brush.Color := clBtnHighLight
    else
      Brush.Color := clBtnFace;

    LTabCaption := PageControl.Pages[TabIndex].Caption;
    LCaptionX := Rect.Left + ((Rect.Right - Rect.Left - TextWidth(LTabCaption)) div 2);
    LCaptionY := Rect.Top + ((Rect.Bottom - Rect.Top - TextHeight('Gg')) div 2);
    FillRect(Rect);
    TextOut(LCaptionX, LCaptionY, LTabCaption);
  end;
end;

procedure TDefaultsForm.PropertyOnChange(Sender: TObject);
begin
  ApplyBtn.Enabled := true;
end;


procedure TDefaultsForm.Init;
var LReg: TRegistry;
begin
  LReg := TRegistry.Create;
  LReg.RootKey := HKEY_CURRENT_USER;
  with LReg do
  try
    if KeyExists('Software\HZB_Helium\Def_Options\') then
    begin
      OpenKey('Software\HZB_Helium\Def_Options\',true);

      if ValueExists('interval_short') then
        ShortIntEd.Text := intToStr(LReg.ReadInteger('interval_short'));
      if LReg.ValueExists('interval_long') then
        LongIntEd.Text := intToStr(LReg.ReadInteger('interval_long'));
      if LReg.ValueExists('resistance_min') then
        MinResEd.Text := floatToStr(LReg.ReadFloat('resistance_min'));
      if LReg.ValueExists('resistance_max') then
        MaxResEd.Text := floatToStr(LReg.ReadFloat('resistance_max'));
      if LReg.ValueExists('heattime') then
        HeatEd.Text := intToStr(LReg.ReadInteger('heattime'));
      if LReg.ValueExists('adc_cycles') then
        AdcEd.Text := intToStr(LReg.ReadInteger('adc_cycles'));
      if LReg.ValueExists('fill_timeout') then
        FillTimeEd.Text := intToStr(LReg.ReadInteger('fill_timeout'));
      if LReg.ValueExists('span') then
        SpanEd.Text := intToStr(LReg.ReadInteger('span'));
      if LReg.ValueExists('zero') then
        ZeroEd.Text := intToStr(LReg.ReadInteger('zero'));

      if LReg.ValueExists('voltage_max') then
        VoltMaxEd.Text := floatToStr(LReg.ReadFloat('voltage_max'));
      if LReg.ValueExists('voltage_min') then
        VoltMinEd.Text := floatToStr(LReg.ReadFloat('voltage_min'));
      if LReg.ValueExists('voltage_alarm') then
        VoltAlarmEd.Text := intToStr(LReg.ReadInteger('voltage_alarm'));
    end;
  finally
    LReg.CloseKey;
  end;
  LReg.Free;
end;

procedure TDefaultsForm.ApplyValues;
begin
  if (ShortIntEd.Text  <> '') and (strtoint(ShortIntEd.Text)  > 0) then
   if (LongIntEd.Text   <> '') and (strtoint(LongIntEd.Text)   > 0) then
    if (MinResEd.Text    <> '') and (strtofloat(MinResEd.Text)  > 0) then
     if (MaxResEd.Text    <> '') and (strtofloat(MaxResEd.Text)  > 0) then
      if (HeatEd.Text      <> '') and (strtoint(HeatEd.Text)      > 0) then
       if (AdcEd.Text       <> '') and (strtoint(AdcEd.Text)       > 0) then
        if (FillTimeEd.Text  <> '') and (strtoint(FillTimeEd.Text)  > 0) then
         if (SpanEd.Text      <> '') and (strtofloat(SpanEd.Text)    > 0) then
          if (ZeroEd.Text      <> '') and (strtofloat(ZeroEd.Text)    > 0) then
           if (VoltMaxEd.Text   <> '') and (strtofloat(VoltMaxEd.Text) > 0) then
            if (VoltMinEd.Text   <> '') and (strtofloat(VoltMinEd.Text) > 0) then
             if (VoltAlarmEd.Text <> '') and (strtoint(VoltAlarmEd.Text) >= 0)
                                         and (strtoint(VoltAlarmEd.Text) <= 100) then
             begin
               if (MessageDlg('Do you really want to save?',
                              mtCustom, [mbYes, mbNo], 0) = mrYes) then
               begin
                 SaveValues;
                 ApplyBtn.Enabled := false;
               end;
             end
             else MessageDlg('Invalid value for critical battery capacity (0-100 %)!', mtError, [mbOK], 0)
            else MessageDlg('Invalid value for Min voltage!', mtError, [mbOK], 0)
           else MessageDlg('Invalid value for Max voltage!', mtError, [mbOK], 0)
          else MessageDlg('Invalid value for zero!', mtError, [mbOK], 0)
         else MessageDlg('Invalid value for span!', mtError, [mbOK], 0)
        else MessageDlg('Invalid fill timeout!', mtError, [mbOK], 0)
       else MessageDlg('Invalid number of adc loops!', mtError, [mbOK], 0)
      else MessageDlg('Invalid heat time!', mtError, [mbOK], 0)
     else MessageDlg('Invalid max resistance!', mtError, [mbOK], 0)
    else MessageDlg('Invalid min resistance!', mtError, [mbOK], 0)
   else MessageDlg('Invalid long interval!', mtError, [mbOK], 0)
  else MessageDlg('Invalid short interval!', mtError, [mbOK], 0);
end;

procedure TDefaultsForm.SaveValues;
var
  LReg: TRegistry;
begin
  LReg := TRegistry.Create;
  LReg.RootKey := HKEY_CURRENT_USER;
  try
    LReg.OpenKey('Software\HZB_Helium\Def_Options\',true);

    LReg.WriteInteger('interval_short', strToInt(ShortIntEd.Text));
    LReg.WriteInteger('interval_long',  strToInt(LongIntEd.Text));
    LReg.WriteFloat('resistance_min',   strToFloat(MinResEd.Text));
    LReg.WriteFloat('resistance_max',   strToFloat(MaxResEd.Text));
    LReg.WriteInteger('heattime',       strToInt(HeatEd.Text));
    LReg.WriteInteger('adc_cycles',     strToInt(AdcEd.Text));
    LReg.WriteInteger('fill_timeout',   strToInt(FillTimeEd.Text));
    LReg.WriteInteger('span',           strToInt(SpanEd.Text));
    LReg.WriteInteger('zero',           strToInt(ZeroEd.Text));
    LReg.WriteFloat('voltage_max',      strToFloat(VoltMaxEd.Text));
    LReg.WriteFloat('voltage_min',      strToFloat(VoltMinEd.Text));
    LReg.WriteInteger('voltage_alarm',  strToInt(VoltAlarmEd.Text));
  finally
    LReg.CloseKey;
  end;
  LReg.Free;
end;


procedure TDefaultsForm.cOkBtnClick(Sender: TObject);
begin
  if ApplyBtn.Enabled then
    ApplyValues;

  if not ApplyBtn.Enabled then
    ModalResult := mrOK;
end;

procedure TDefaultsForm.CancelBtnClick(Sender: TObject);
begin
  ModalResult := mrCancel;
  if ApplyBtn.Enabled then
    if (MessageDlg('If you cancel your changes will get lost.' + #13
                   + 'Do you want to cancel anyway?',
                   mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
      ApplyBtn.Enabled := false
    else
      ModalResult := mrNone;
end;

procedure TDefaultsForm.ApplyBtnClick(Sender: TObject);
begin
  ApplyValues;
end;

end.
