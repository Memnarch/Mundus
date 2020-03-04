program SoftwareRendererProj;

uses
  Forms,
  Main in 'Main.pas' {Form1},
  SoftwareRenderer in 'SoftwareRenderer.pas',
  BaseMesh in 'BaseMesh.pas',
  Cube in 'Cube.pas',
  Math3D in 'Math3D.pas',
  Shader in 'Shader.pas',
  ColorTypes in 'ColorTypes.pas',
  SolidColorShader in 'SolidColorShader.pas',
  DepthColorShader in 'DepthColorShader.pas',
  Interpolation in 'Interpolation.pas',
  TextureShader in 'TextureShader.pas',
  StopWatch in 'StopWatch.pas',
  XMM in 'XMM.pas',
  DrawCall in 'DrawCall.pas',
  RenderWorker in 'RenderWorker.pas',
  Rasterizer in 'Rasterizer.pas',
  SamplerGraph in 'SamplerGraph.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
