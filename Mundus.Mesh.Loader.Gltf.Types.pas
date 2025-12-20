unit Mundus.Mesh.Loader.Gltf.Types;

interface

uses
  Mundus.Material;

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

  TGLTFData = record
    Buffers: TArray<TBuffer>;
    Views: TArray<TBufferView>;
    Accessors: TArray<TAccessor>;
    Images: TArray<TImage>;
    Textures: TArray<TTexture>;
    Materials: TArray<TMaterial>;
  end;

implementation

end.
