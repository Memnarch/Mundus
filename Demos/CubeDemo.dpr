program CubeDemo;

uses
  Forms,
  Main in 'Main.pas' {Form1},
  Mundus.Camera in '..\Mundus.Camera.pas',
  Mundus.Diagnostics.SamplerGraph in '..\Mundus.Diagnostics.SamplerGraph.pas',
  Mundus.Diagnostics.StopWatch in '..\Mundus.Diagnostics.StopWatch.pas',
  Mundus.DrawCall in '..\Mundus.DrawCall.pas',
  Mundus.Math.Interpolation in '..\Mundus.Math.Interpolation.pas',
  Mundus.Math in '..\Mundus.Math.pas',
  Mundus.Mesh.Cube in '..\Mundus.Mesh.Cube.pas',
  Mundus.Mesh in '..\Mundus.Mesh.pas',
  Mundus.Rasterizer in '..\Mundus.Rasterizer.pas',
  Mundus.Renderer in '..\Mundus.Renderer.pas',
  Mundus.Renderer.Worker in '..\Mundus.Renderer.Worker.pas',
  Mundus.Shader.DepthColor in '..\Mundus.Shader.DepthColor.pas',
  Mundus.Shader in '..\Mundus.Shader.pas',
  Mundus.Shader.Texture in '..\Mundus.Shader.Texture.pas',
  Mundus.Shader.VertexGradient in '..\Mundus.Shader.VertexGradient.pas',
  Mundus.Texture in '..\Mundus.Texture.pas',
  Mundus.Types in '..\Mundus.Types.pas',
  Mundus.ValueBuffer in '..\Mundus.ValueBuffer.pas',
  Mundus.Mesh.Plane in '..\Mundus.Mesh.Plane.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
