unit GameEngine;

interface

uses
  Winapi.Windows,
  System.Classes, System.Types,
  Vcl.Graphics, Vcl.Imaging.pngimage,
  CollisionsDetection;

const
  C_GRAVITY = 1;

type
  TAsset = class
  strict private
    FBitmap: TBitmap;
    FOffsets: array of Integer;
    FWidth: Integer;
    FHeight: Integer;
  private
    function GetHandle: THandle;
    function GetOffset(Index: Integer): Integer;
  public
    destructor Destroy; override;
    procedure LoadAsset(const AResource: string; ASize: TSize; ACount: Integer = 1);
    property Offset[&Index: Integer]: Integer read GetOffset;
    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
    property Handle: THandle read GetHandle;
  end;

  TRigidbody = class
    Mass: Single;
    Velocity: TVector;
  end;

  TGameObject = class
  strict private
    FChildren: TList;
    FRigidbody: TRigidbody;
    FColliders: TList;
  strict protected
    FParent: TGameObject;
  protected
    procedure Update; virtual;
  public
    Position: TPointF;
    constructor Create;
    destructor Destroy; override;
    procedure AddChild(AChild: TGameObject);
    property Rigidbody: TRigidbody read FRigidbody;
    procedure AddRigidbody(AMass: Integer);
    property Colliders: TList read FColliders;
    procedure AddCollider(ACollider: TCollider);
  end;

  TSprite = class(TGameObject)
    Asset: TAsset;
    CurrentFrame: Integer;
    procedure Draw(ACanvas: TCanvas);
  end;

  TGameEngine = class
  strict private
    FAssets: TStrings;
    FPrivateFonts: TList;
    FGameObjects: TList;
    function GetAsset(const &Name: string): TAsset;
  public
    Width: Integer;
    Height: Integer;
    IsGameOver: Boolean;
    constructor Create;
    destructor Destroy; override;
    function CreateAsset(const AResource: string; ASize: TSize; ACount: Integer = 1): TAsset;
    function CreateFont(const AResource: string): TFont;
    function CreateSprite<T: TSprite, constructor>(AParent: TGameObject = nil): T;
    procedure UpdateScene;
    procedure DrawScene(ACanvas: TCanvas);
    property Asset[const &Name: string]: TAsset read GetAsset;
    property GameObjects: TList read FGameObjects;
  end;

var
  GameEngine1: TGameEngine;

implementation

{ TAsset }

procedure TAsset.LoadAsset(const AResource: string; ASize: TSize; ACount: Integer);
var
  LPngImage: TPngImage;
  LBitmap: TBitmap;
  I: Integer;
begin
  FWidth := ASize.cx;
  FHeight := ASize.cy;
  LPngImage := TPngImage.Create;
  LPngImage.LoadFromResourceName(HInstance, AResource);
  LBitmap := TBitmap.Create;
  LBitmap.Assign(LPngImage);
  LPngImage.Free;
  FBitmap := TBitmap.Create;
  FBitmap.PixelFormat := LBitmap.PixelFormat;
  FBitmap.AlphaFormat := LBitmap.AlphaFormat;
  FBitmap.Canvas.Brush.Style := bsClear;
  FBitmap.SetSize(FWidth * ACount, FHeight);
  FBitmap.Canvas.StretchDraw(Rect(0, 0, FBitmap.Width, FBitmap.Height), LBitmap);
  LBitmap.Free;
  SetLength(FOffsets, ACount);
  for I := 0 to Pred(Length(FOffsets)) do
    FOffsets[I] := I * FWidth;
end;

destructor TAsset.Destroy;
begin
  FBitmap.Free;
  inherited;
end;

function TAsset.GetHandle: THandle;
begin
  Result := FBitmap.Canvas.Handle;
end;

function TAsset.GetOffset(Index: Integer): Integer;
begin
  Result := FOffsets[&Index];
end;

{ TGameObject }

constructor TGameObject.Create;
begin
  FColliders := TList.Create;
  FColliders.Capacity := 1;
end;

destructor TGameObject.Destroy;
var
  LPointer: Pointer;
begin
  if FChildren <> nil then
    FChildren.Free;
  if FRigidbody <> nil then
    FRigidbody.Free;
  for LPointer in FColliders do
    TCollider(LPointer).Free;
  FColliders.Free;
  inherited;
end;

procedure TGameObject.AddChild(AChild: TGameObject);
begin
  if FChildren = nil then
    FChildren := TList.Create;
  FChildren.Add(AChild);
  AChild.FParent := Self;
end;

procedure TGameObject.AddRigidbody(AMass: Integer);
begin
  FRigidbody := TRigidbody.Create;
  FRigidbody.Mass := AMass;
end;

procedure TGameObject.AddCollider(ACollider: TCollider);
begin
  ACollider.GameObject := Self;
  FColliders.Add(ACollider);
end;

procedure TGameObject.Update;
begin
  if Assigned(Rigidbody) then
  begin
    Rigidbody.Velocity.Y := Rigidbody.Velocity.Y + (Rigidbody.Mass * C_GRAVITY);
    Position.Offset(Rigidbody.Velocity.ToPointF);
  end;
end;

{ TSprite }

procedure TSprite.Draw(ACanvas: TCanvas);
const
  C_BLENDFUNCTION: TBlendFunction = (BlendOp: AC_SRC_OVER; SourceConstantAlpha: 255; AlphaFormat: AC_SRC_ALPHA);
var
  LPoint: TPoint;
begin
  if FParent = nil then
    LPoint := Position.Round
  else
    LPoint := (FParent.Position + Position).Round;
  AlphaBlend(ACanvas.Handle, LPoint.X, LPoint.Y, Asset.Width, Asset.Height, Asset.Handle, Asset.Offset[CurrentFrame], 0, Asset.Width, Asset.Height, C_BLENDFUNCTION);
end;

{ TGameEngine }

constructor TGameEngine.Create;
begin
  FAssets := TStringList.Create(True);
  FPrivateFonts := TList.Create;
  FGameObjects := TList.Create;
end;

destructor TGameEngine.Destroy;
var
  LPointer: Pointer;
begin
  for LPointer in FGameObjects do
    TGameObject(LPointer).Free;
  FGameObjects.Free;
  for LPointer in FPrivateFonts do
    TFont(LPointer).Free;
  FPrivateFonts.Free;
  FAssets.Free;
  inherited;
end;

function TGameEngine.CreateAsset(const AResource: string; ASize: TSize; ACount: Integer): TAsset;
begin
  Result := TAsset.Create;
  Result.LoadAsset(AResource, ASize, ACount);
  FAssets.AddObject(AResource, Result);
end;

function TGameEngine.GetAsset(const &Name: string): TAsset;
begin
  Result := TAsset(FAssets.Objects[FAssets.IndexOf(&Name)]);
end;

function TGameEngine.CreateFont(const AResource: string): TFont;
var
  LResourceStream: TResourceStream;
  LFontsCount: Integer;
begin
  LResourceStream := TResourceStream.Create(HInstance, AResource, RT_RCDATA);
  Result := TFont.Create;
  Result.Handle := AddFontMemResourceEx(LResourceStream.Memory, LResourceStream.Size, nil, @LFontsCount);
  LResourceStream.Free;
  FPrivateFonts.Add(Result);
end;

function TGameEngine.CreateSprite<T>(AParent: TGameObject = nil): T;
begin
  Result := T.Create;
  if Assigned(AParent) then
    AParent.AddChild(Result);
  FGameObjects.Add(Pointer(Result));
end;

procedure TGameEngine.UpdateScene;
var
  LGameObject: Pointer;
begin
  for LGameObject in FGameObjects do
    TGameObject(LGameObject).Update;
end;

procedure TGameEngine.DrawScene(ACanvas: TCanvas);
var
  LGameObject: Pointer;
begin
  for LGameObject in FGameObjects do
    if TGameObject(LGameObject) is TSprite then
      TSprite(LGameObject).Draw(ACanvas);
end;

initialization

GameEngine1 := TGameEngine.Create;

finalization

GameEngine1.Free;

end.
