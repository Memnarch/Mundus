program CubeDemo;

uses
  Forms,
  Main in 'Main.pas' {Form1},
  Mundus.Camera in '..\Mundus.Camera.pas',
  Mundus.Diagnostics.SamplerGraph in '..\Mundus.Diagnostics.SamplerGraph.pas',
  Mundus.Diagnostics.StopWatch in '..\Mundus.Diagnostics.StopWatch.pas',
  Mundus.DrawCall in '..\Mundus.DrawCall.pas',
  Mundus.Material in '..\Mundus.Material.pas',
  Mundus.Math.Interpolation in '..\Mundus.Math.Interpolation.pas',
  Mundus.Math in '..\Mundus.Math.pas',
  Mundus.Mesh.Cube in '..\Mundus.Mesh.Cube.pas',
  Mundus.Mesh.Loader.Obj in '..\Mundus.Mesh.Loader.Obj.pas',
  Mundus.Mesh.Loader in '..\Mundus.Mesh.Loader.pas',
  Mundus.Mesh in '..\Mundus.Mesh.pas',
  Mundus.Mesh.Plane in '..\Mundus.Mesh.Plane.pas',
  Mundus.PixelBuffer in '..\Mundus.PixelBuffer.pas',
  Mundus.Rasterizer in '..\Mundus.Rasterizer.pas',
  Mundus.Renderer.Clipping in '..\Mundus.Renderer.Clipping.pas',
  Mundus.Renderer in '..\Mundus.Renderer.pas',
  Mundus.Renderer.Worker in '..\Mundus.Renderer.Worker.pas',
  Mundus.Shader.DepthColor in '..\Mundus.Shader.DepthColor.pas',
  Mundus.Shader.Occlusion in '..\Mundus.Shader.Occlusion.pas',
  Mundus.Shader in '..\Mundus.Shader.pas',
  Mundus.Shader.Texture in '..\Mundus.Shader.Texture.pas',
  Mundus.Shader.VertexGradient in '..\Mundus.Shader.VertexGradient.pas',
  Mundus.ShaderCache in '..\Mundus.ShaderCache.pas',
  Mundus.Texture in '..\Mundus.Texture.pas',
  Mundus.Types in '..\Mundus.Types.pas',
  Mundus.ValueBuffer in '..\Mundus.ValueBuffer.pas' {$R *.res},
  Mundus.Rasterizer.Helper in '..\Mundus.Rasterizer.Helper.pas',
  Mundus.Rasterizer.Types in '..\Mundus.Rasterizer.Types.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
