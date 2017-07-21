program HeliumManagement;

uses
  madExcept,
  madLinkDisAsm,
  madListHardware,
  madListProcesses,
  madListModules,
  Forms,
  HeliumMain in 'HeliumMain.pas' {MainForm},
  XBeeCoordinator in 'XBeeCoordinator.pas' {CoordinatorForm},
  HeliumFunctions in 'HeliumFunctions.pas',
  XBeeModule in 'XBeeModule.pas' {ModuleForm},
  Vessel in 'Vessel.pas' {VesselForm},
  About in 'About.pas' {AboutForm},
  OptionsVessel in 'OptionsVessel.pas' {OptionsForm},
  Settings in 'Settings.pas' {SettingsForm},
  NichtDoppeltStarten in 'NichtDoppeltStarten.pas',
  OptionsModule in 'OptionsModule.pas' {ModuleOptionForm},
  VesselType in 'VesselType.pas' {VesselTypeForm},
  HeliumDataMgmt in 'HeliumDataMgmt.pas',
  Defaults in 'Defaults.pas' {DefaultsForm},
  TextMsg in 'TextMsg.pas' {TextMsgForm},
  EvaluateDb in 'EvaluateDb.pas',
  IlmDevice in 'IlmDevice.pas' {IlmDeviceForm},
  SQLClientInterfaces in 'SQLClientInterfaces.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
