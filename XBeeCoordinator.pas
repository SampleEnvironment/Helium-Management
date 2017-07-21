unit XBeeCoordinator;

interface

uses
  SysUtils, Classes, Controls, Forms, Dialogs, StdCtrls, ComCtrls, ExtCtrls,
  HeliumDataMgmt, DatabaseEdit;

type
  TCoordinatorForm = class(TForm)
    cooCoBox: TComboBox;
    cooPanel: TPanel;
    NameLab: TLabel;
    NameEd: TOraEdit;
    IpLab: TLabel;
    IpEd: TOraEdit;
    AddressLab: TLabel;
    AddressEd: TOraEdit;
    PositionLab: TLabel;
    PositionEd: TOraEdit;
    CommentRiEd: TRichEdit;
    CommentLab: TLabel;
    PortCoBox: TComboBox;
    PortLab: TLabel;
    cooLab: TLabel;
    SaveBtn: TButton;
    DeleteBtn: TButton;
    HintLab: TLabel;
    ActiveLab: TLabel;
    CancelBtn: TButton;
    procedure cooCoBoxChange(Sender: TObject);
    procedure SaveBtnClick(Sender: TObject);
    procedure DeleteBtnClick(Sender: TObject);
    procedure PortCoBoxChange(Sender: TObject);
  private
    FDataMgmt: TDataMgmt;
    FPortList: TStringList;

    procedure InitForm;
    function  FillCoBox(APort: string): TStringList;
  public
    constructor CreateWithProperties(ADataMgmt: TDataMgmt; AOwner: TComponent);
    destructor  Destroy; override;
  end;

implementation
uses HeliumFunctions;
{$R *.dfm}

constructor TCoordinatorForm.CreateWithProperties(ADataMgmt: TDataMgmt; AOwner: TComponent);
begin
  inherited Create(AOwner);

  HintLab.Caption   := '';
  FDataMgmt         := ADataMgmt;
  FPortList         := TStringList.Create;
  InitForm;
end;

destructor TCoordinatorForm.Destroy;
begin
  FPortList.Free;
  
  inherited Destroy;
end;

procedure TCoordinatorForm.InitForm;
var i: integer;
begin
  FPortList.Clear;
  PortCoBox.Clear;
  CooCoBox.Clear;
  CooCoBox.AddItem('- CREATE NEW COORDINATOR -', nil);

  with FDataMgmt do
  try
    LockWhileWorkingWithList;
    RefreshCooListByDB;
    for i := 0 to CoordinatorList.Count - 1 do
    begin
      FPortList.Add(   TCoordinator(CoordinatorList[i]).coo_comport);
      CooCoBox.AddItem(TCoordinator(CoordinatorList[i]).coo_name,
                       TObject(TCoordinator(CoordinatorList[i]).coo_id));
    end;
  finally
    UnlockAfterWorkingWithList;
  end;

  ActiveLab.Caption  := '';
  NameEd.Text        := '';
  IpEd.Text          := '';
  AddressEd.Text     := '';
  PositionEd.Text    := '';
  CommentRiEd.Text   := '';

  IpEd.Enabled       := true;
  AddressEd.Enabled  := true;
  PortCoBox.Enabled  := true;
  DeleteBtn.Enabled  := true;

  CooCoBox.ItemIndex := 0;
  ActiveControl      := CooCoBox;
  cooCoBoxChange(nil);
end;


function TCoordinatorForm.FillCoBox(APort: string): TStringList;
var i: integer;
begin
  Result := GetPortList();

  for i := Result.Count - 1 downto 0 do
    if (Result[i] <> APort) and ((FPortList.IndexOf(Result[i])) > -1)
     then Result.Delete(i);
end;


procedure TCoordinatorForm.cooCoBoxChange(Sender: TObject);
var i:    integer;
    LCoo: TCoordinator;
begin
  if Assigned(Sender) then HintLab.Caption := '';
  PortCoBox.Clear;
  IpEd.Enabled      := true;
  AddressEd.Enabled := true;
  PortCoBox.Enabled := true;
  DeleteBtn.Enabled := true;

  if (cooCoBox.ItemIndex = 0) then
  begin
    ActiveLab.Caption := 'Enter values for the new coordinator.';
    NameEd.Text       := '';
    IpEd.Text         := '';
    AddressEd.Text    := '';
    PositionEd.Text   := '';
    CommentRiEd.Text  := '';
    PortCoBox.Items   := FillCoBox('');
  end;

  if (cooCoBox.ItemIndex > 0) then
  begin
    with FDataMgmt, cooCoBox do
    try
      LockWhileWorkingWithList;
      LCoo := GetCopyOfCoordinator(GetCooIndex(integer(Items.Objects[ItemIndex])));
    finally
      UnlockAfterWorkingWithList;
    end;

    if Assigned(LCoo) then
    begin
      if (LCoo.coo_status = 0) then
      begin
        ActiveLab.Caption := '';
      end
      else begin
        IpEd.Enabled      := false;
        AddressEd.Enabled := false;
        PortCoBox.Enabled := false;
        DeleteBtn.Enabled := false;
        ActiveLab.Caption := cooCoBox.Text + ' is currently active!';
      end;

      NameEd.Text      := LCoo.coo_name;
      IpEd.Text        := LCoo.coo_ip;
      AddressEd.Text   := LCoo.coo_address;
      PositionEd.Text  := LCoo.coo_position;
      CommentRiEd.Text := LCoo.coo_comment;
      PortCoBox.Items  := FillCoBox(LCoo.coo_comport);

      for i := 0 to PortCoBox.Items.Count - 1 do
       if (LCoo.coo_comport = PortCoBox.Items[i]) then
       begin
         PortCoBox.ItemIndex := i;
         break;
       end;

      if (PortCoBox.ItemIndex = -1)
       then HintLab.Caption := 'Port ' + LCoo.coo_comport + ' is not installed!';
    end
    else InitForm;
  end;
end;

procedure TCoordinatorForm.PortCoBoxChange(Sender: TObject);
begin
  HintLab.Caption := '';
end;


procedure TCoordinatorForm.SaveBtnClick(Sender: TObject);
var LCoo: TCoordinator;
begin
  LCoo := nil;
  if (cooCoBox.ItemIndex    <> -1) then
   if (trim(NameEd.Text)     <> '') then
    if (PortCoBox.ItemIndex   <> -1) then
     if (trim(IpEd.Text)       <> '') then
      if (trim(AddressEd.Text)  <> '') then
       if (trim(PositionEd.Text) <> '') then
       begin
         if (cooCoBox.ItemIndex = 0) then 
          if (MessageDlg('Do you really want to save the new coordinator?', mtCustom, [mbYes, mbNo], 0) = mrYes) then
            with FDataMgmt do
            try
              LockWhileWorkingWithList;
              LCoo              := TCoordinator.Create;
              LCoo.coo_id       := 0;
              LCoo.coo_name     := NameEd.Text;
              LCoo.coo_comport  := PortCoBox.Text;
              LCoo.coo_ip       := IpEd.Text;
              LCoo.coo_address  := AddressEd.Text;
              LCoo.coo_position := PositionEd.Text;
              LCoo.coo_comment  := CommentRiEd.Text;

              if InsertCoordinatorToDb(LCoo)
               then HintLab.Caption := 'Creating new coordinator succeeded.'
               else HintLab.Caption := 'Creating new coordinator failed.';
            finally
              UnlockAfterWorkingWithList;
              InitForm;
            end;

         if (cooCoBox.ItemIndex > 0) then
          if (MessageDlg('Do you really want to change the coordinator settings?', mtCustom, [mbYes, mbNo], 0) = mrYes) then
            with FDataMgmt, cooCoBox do
            try
              LockWhileWorkingWithList;
              LCoo := GetCooItem(integer(Items.Objects[ItemIndex]));

              if Assigned(LCoo) then
              begin
                LCoo.coo_name     := NameEd.Text;
                LCoo.coo_comport  := PortCoBox.Text;
                LCoo.coo_ip       := IpEd.Text;
                LCoo.coo_address  := AddressEd.Text;
                LCoo.coo_position := PositionEd.Text;
                LCoo.coo_comment  := CommentRiEd.Text;

                if UpdateCoordinatorToDb(LCoo)
                 then HintLab.Caption := 'Update on ' + cooCoBox.Text + ' succeeded.'
                 else HintLab.Caption := 'Update on ' + cooCoBox.Text + ' failed.';
              end
              else HintLab.Caption := 'Update on ' + cooCoBox.Text + ' failed.';
            finally
              LCoo := nil;
              UnlockAfterWorkingWithList;
              InitForm;
            end;
       end
       else showMessage('Please enter the position of the coordinator!')
      else showMessage('Please enter the XBee address of the coordinator!')
     else showMessage('Please enter the IP address of the coordinator (netnode)!')
    else showMessage('Please select a virtual COM port for the coordinator!')
   else showMessage('Please enter a name (netnode) for the coordinator!');

  if Assigned(LCoo) then LCoo.Free;
end;

procedure TCoordinatorForm.DeleteBtnClick(Sender: TObject);
begin
  if (cooCoBox.ItemIndex > 0) then
  begin
    if (MessageDlg('Do you really want to delete ' + cooCoBox.Text + '?', mtCustom, [mbYes, mbNo], 0) = mrYes) then
     with FDataMgmt, cooCoBox do
     try
       LockWhileWorkingWithList;
       if DeleteCoordinatorToDb(integer(Items.Objects[ItemIndex]))
        then HintLab.Caption := cooCoBox.Text + ' successfully deleted.';
     finally
       UnlockAfterWorkingWithList;
       InitForm;
     end;
  end
  else showMessage('Select a coordinator to delete!');
end;

end.
