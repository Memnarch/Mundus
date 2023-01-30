unit Mundus.Texture;

interface

uses
  Classes,
  Types,
  Graphics,
  Mundus.Types,
  Math;

{$IFDEF Debug}
  {$Inline off}
{$ENDIF}

type
  TTexture = class
  private
    FBitmap: TBitmap;
    FFirst: PRGB32Array;
    FLineLengthInPixel: Integer;
    FWidthMask: Integer;
    FHeightMask: Integer;
    function GetHeight: Integer;
    function GetWidth: Integer;
    function GetMaxX: Integer;
    function GetMaxY: Integer;
    procedure CalculateMaskAndSize(AValue: Integer; out AMask, ADesiredSize: Integer);
    procedure Resize(ANewWidth, ANewHeight: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromFile(const AFile: string);
    procedure SampleDot(const AUV: tfloat2; const ATarget: PRGB32);
    property Width: Integer read GetWidth;
    property Height: Integer read GetHeight;
    property MaxX: Integer read GetMaxX;
    property MaxY: Integer read GetMaxY;
  end;

implementation

uses
  jpeg,
  PngImage,
  SysUtils;

{ TTexture }

procedure TTexture.CalculateMaskAndSize(AValue: Integer; out AMask, ADesiredSize: Integer);
var
  LValue, LHighestBit, LBit, i: Integer;
  LNotPOT: Boolean;
begin
  AMask := 0;
  LNotPOT := False;

  LValue := AValue;

  LBit := 0;
  LHighestBit := -1;
  while (LValue > 0) do
  begin
    if LValue and 1 = 1 then
    begin
      if LHighestBit > -1 then
        LNotPOT := True;
      LHighestBit := LBit
    end;
    Inc(LBit);
    LValue := LValue shr 1;
  end;

  if LNotPOT then
    Inc(LHighestBit);

  for i := 1 to LHighestBit do
  begin
    AMask := AMask shl 1;
    Inc(AMask);
  end;

  ADesiredSize := AMask + 1;
end;

constructor TTexture.Create;
begin
  inherited;
  FBitmap := TBitmap.Create();
end;

destructor TTexture.Destroy;
begin
  FBitmap.Free;
  inherited;
end;

function TTexture.GetHeight: Integer;
begin
  Result := FBitmap.Height;
end;


function TTexture.GetMaxX: Integer;
begin
  Result := FBitmap.Width - 1;
end;

function TTexture.GetMaxY: Integer;
begin
  Result := FBitmap.Height - 1;
end;

function TTexture.GetWidth: Integer;
begin
  Result := FBitmap.Width;
end;

procedure TTexture.LoadFromFile(const AFile: string);
var
  LPicture: TPicture;
  LDesiredWidth, LDesiredHeight: Integer;
begin
  LPicture := TPicture.Create();
  try
    LPicture.LoadFromFile(AFile);
    FBitmap.Assign(LPicture.Graphic);
  finally
    LPicture.Free;
  end;

  FBitmap.PixelFormat := pf32bit;

  CalculateMaskAndSize(FBitmap.Width, FWidthMask, LDesiredWidth);
  CalculateMaskAndSize(FBitmap.Height, FHeightMask, LDesiredHeight);
  if (LDesiredWidth <> FBitmap.Width) or (LDesiredHeight <> FBitmap.Height) then
    Resize(LDesiredWidth, LDesiredHeight);

  FFirst := FBitmap.ScanLine[FHeightMask];
  FLineLengthInPixel := (Longint(FBitmap.Scanline[FHeightMask-1]) - Longint(FFirst)) div SizeOf(TRGB32);
end;

procedure TTexture.Resize(ANewWidth, ANewHeight: Integer);
var
  LTemp: TBitmap;
begin
  LTemp := TBitmap.Create();
  try
    LTemp.Assign(FBitmap);
    FBitmap.SetSize(ANewWidth, ANewHeight);
    FBitmap.Canvas.StretchDraw(FBitmap.Canvas.ClipRect, LTemp);
  finally
    LTemp.Free;
  end;
end;

procedure TTexture.SampleDot(const AUV: TUV; const ATarget: PRGB32);
asm
  //eax = Self
  //edx = AUV
  //ecx = ATarget

  //save ebx to get another scratch register
  push ebx

  //mov U(X) & V(Y) into simd registers
  movss xmm0, [edx + TUV.U]
  movss xmm1, [edx + TUV.V]

  //Truncate V and store in edx
  cvttss2si edx, xmm1
  //wrap V by Height
  and edx, [eax + TTexture.FHeightMask]
  //calculate total number of pixel on V
  imul edx, [eax + TTexture.FLineLengthInPixel]

  //Truncate U and store in ecx
  cvttss2si ebx, xmm0
  //Wrap by Width
  and ebx, [eax + TTexture.FWidthMask]
  //SUM U/V position in Texture
  add ebx, edx

  //dereference to FFirst
  mov eax, [eax + TTexture.FFirst]
  //copy pixel at calculate location i.e. FFirst[Index]. 4 is the size of TRGB32 in bytes
  mov ebx, [eax + ebx*4]
  //copy pixelvalues to Target
  mov [ATarget], ebx

  //restore ebx
  pop ebx
end;

end.
