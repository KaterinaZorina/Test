program PTest;

uses
  Vcl.Forms,
  UFMain1v in '..\..\..\projects\UFMain1v.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
