unit BaseMesh;

interface

uses
  Types, Classes, SysUtils, Generics.Collections;

type
  TVector = record
    X, Y, Z: Integer;
  end;

  TTriangle = record
    VertexA, VertexB, VertexC: Integer;
  end;

  TUV = record
    U, V: Single;
  end;

  TVectorClass = class
  private
    FX, FY, FZ: Integer;
  public
    constructor Create(AX, AY, AZ: Integer);
    property X: Integer read FX write FX;
    property Y: Integer read FY write FY;
    property Z: Integer read FZ write FZ;
  end;

  TTriangleClass = class
  private
    FVertexA, FVertexB, FVertexC: Integer;
    FUVB: TUV;
    FUVC: TUV;
    FUVA: TUV;
  public
    constructor Create(AVertexA, AvertexB, AvertexC: Integer);
    procedure SetUV(AUVA, AUVB, AUVC: TUV);
    property VertexA: Integer read FVertexA write FVertexA;
    property VertexB: Integer read FVertexB write FVertexB;
    property VertexC: Integer read FVertexC write FVertexC;
    property UVA: TUV read FUVA write FUVA;
    property UVB: TUV read FUVB write FUVB;
    property UVC: TUV read FUVC write FUVC;
  end;

  TBaseMesh = class
  private
    FVertexList: TObjectList<TVectorClass>;
    FTriangleList: TObjectList<TTriangleClass>;
    FPosition: TVector;
  public
    constructor Create();
    destructor Destroy(); override;
    property Triangles: TObjectList<TTriangleClass> read FTriangleList;
    property Vertices: TObjectList<TVectorClass> read FVertexList;
    property Position: TVector read FPosition write FPosition;
  end;

  function Vector(AX, AY, AZ: Integer): TVector;
  function UV(AU, AV: Single): TUV;
  function Triangle(AVertexA, AVertexB, AVertexC: Integer): TTriangle;

implementation

{ TBaseMesh }

constructor TBaseMesh.Create;
begin
  FVertexList := TObjectList<TVectorClass>.Create();
  FTriangleList := TObjectList<TTriangleClass>.Create();
end;

destructor TBaseMesh.Destroy;
begin
  FVertexList.Free();
  FTriangleList.Free();
  inherited;
end;

{ TTriangleClass }

constructor TTriangleClass.Create(AVertexA, AvertexB, AvertexC: Integer);
begin
  FVertexA := AVertexA;
  FVertexB := AvertexB;
  FVertexC := AvertexC;
  FUVA :=  UV(0, 0);
  FUVB := UV(0, 0);
  FUVC := UV(0, 0);
end;

procedure TTriangleClass.SetUV(AUVA, AUVB, AUVC: TUV);
begin
  FUVA := AUVA;
  FUVB := AUVB;
  FUVC := AUVC;
end;

{ TVectorClass }

constructor TVectorClass.Create(AX, AY, AZ: Integer);
begin
  FX := AX;
  FY := AY;
  FZ := AZ;
end;

{ Some Functions }
function Vector(AX, AY, AZ: Integer): TVector;
begin
  Result.X := AX;
  Result.Y := AY;
  Result.Z := AZ;
end;

function Triangle(AVertexA, AVertexB, AVertexC: Integer): TTriangle;
begin
  Result.VertexA := AVertexA;
  Result.VertexB := AVertexB;
  Result.VertexC := AVertexC;
end;

function UV(AU, AV: Single): TUV;
begin
  Result.U := AU;
  Result.V := AV;
end;

end.
