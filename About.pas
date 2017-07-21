unit About;

interface

uses
  Windows, Forms, StdCtrls, Controls, Classes, ExtCtrls, ShellAPI, jpeg;

type
  TAboutForm = class(TForm)
    btnOK: TButton;
    Image1: TImage;
    Label3: TLabel;
    Label1: TLabel;
    Label4: TLabel;
    Label2: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    procedure Label2Click(Sender: TObject);
    procedure Label5Click(Sender: TObject);
    procedure Label6Click(Sender: TObject);
    procedure Image1Click(Sender: TObject);
  end;

implementation
{$R *.DFM}

procedure TAboutForm.Image1Click(Sender: TObject);
begin
  ShellExecute(Application.Handle,
               'open',
               PChar('http://www.helmholtz-berlin.de'),
               nil,
               nil,
               SW_ShowNormal);
end;

procedure TAboutForm.Label2Click(Sender: TObject);
begin
  ShellExecute(Application.Handle,
               'open',
               PChar('http://www.helmholtz-berlin.de/user/neutrons/instrumentation/sample-environment'),
               nil,
               nil,
               SW_ShowNormal);
end;

procedure TAboutForm.Label5Click(Sender: TObject);
begin
  ShellExecute(Application.Handle,
               'open',
               PChar('http://www.helmholtz-berlin.de/user/neutrons/instrumentation/laboratories/lammb'),
               nil,
               nil,
               SW_ShowNormal);
end;

procedure TAboutForm.Label6Click(Sender: TObject);
begin
  ShellExecute(Application.Handle,
               'open',
               PChar('http://www.helmholtz-berlin.de/user/neutrons/instrumentation/laboratories/degas'),
               nil,
               nil,
               SW_ShowNormal);
end;

end.
