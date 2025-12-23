unit Mundus.Mesh.Loader.Gltf.Types;

interface

uses
  Mundus.Material,
  Mundus.Math;

type
  TBuffer = record
    Data: TArray<Byte>;
  end;

  PBuffer = ^TBuffer;

  TBufferView = record
    BufferIndex: Integer;
    Length: Int64;
    Offset: Int64;
  end;

  PBufferView = ^TBufferView;

  TImage = record
    MimeType: string;
    Name: string;
    Uri: string;
  end;

  PImage = ^TImage;

  TTexture = record
    Source: Integer;
  end;

  PTexture = ^TTexture;

  TElementType = (etScalar, etVec2, etVec3, etVec4, etMat2, etMat3, etMat4);
  TComponentType = (ctByte, ctUByte, ctShort, ctUShort, ctUInt32, ctFloat32);

  TAccessor = record
    BufferView: Integer;
    Offset: Int64;
    Count: Integer;
    ComponentType: TComponentType;
    ElementType: TElementType;
  end;

  PAccessor = ^TAccessor;

  TNode = record
    Name: string;
    Children: TArray<Integer>;
    Skin: Integer;
    Mesh: Integer;
    Translation: TFloat3;
    Rotation: TQuaternion;
    Scale: TFloat3;
  end;

  PNode = ^TNode;

  TSkin = record
    Name: string;
    InverseBindMatrices: Integer;
    Joints: TArray<Integer>;
  end;

  PSkin = ^TSkin;

  TTarget = record
    Node: Integer;
    Path: string;
  end;

  PTarget = ^TTarget;

  TChannel = record
    Sampler: Integer;
    Target: TTarget;
  end;

  PChannel = ^TChannel;

  TInterpolation = (iLinear, iStep, iCubicSpline);

  TSampler = record
    Input: Integer;
    Interpolation: TInterpolation;
    Output: Integer;
  end;

  PSampler = ^TSampler;

  TAnimation = record
    Name: string;
    Channels: TArray<TChannel>;
    Samplers: TArray<TSampler>;
  end;

  PAnimation = ^TAnimation;

  TGLTFData = record
    Buffers: TArray<TBuffer>;
    Views: TArray<TBufferView>;
    Accessors: TArray<TAccessor>;
    Images: TArray<TImage>;
    Textures: TArray<TTexture>;
    Materials: TArray<TMaterial>;
    Nodes: TArray<TNode>;
    Skins: TArray<TSkin>;
    Animations: TArray<TAnimation>;
  end;

  TJointIndices = array[0..3] of Byte;

implementation

end.
