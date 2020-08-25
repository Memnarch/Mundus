unit Mundus.Rasterizer.Types;

interface

type
  THalfEdgeConstants = packed record
    C1, C2, C3: Integer;
  end;

  PHalfEdgeConstants = ^THalfEdgeConstants;

  THalfSpaceDeltas = packed record
    X12, X23, X31, _XPadding: Integer;
    Y12, Y23, Y31, _YPadding: Integer;
  end;

  PHalfSpaceDeltas = ^THalfSpaceDeltas;

  TBlockCorners = packed record
    X0, X1: Integer;
    Y0, Y1: Integer;
  end;

  PBlockCorners = ^TBlockCorners;

  TBlockState = packed record
    Intersects: Boolean;
    IsFullBlock: Boolean;
  end;

  PBlockState = ^TBlockState;

  TTrianglePosition = packed record
    _1, _2, _3, _Padding: Integer;
  end;

  PTrianglePosition = ^TTrianglePosition;

implementation

end.
