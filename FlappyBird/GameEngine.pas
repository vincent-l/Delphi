unit GameEngine;

interface

uses
  Winapi.Windows,
  System.Classes, System.Types,
  Vcl.Graphics, Vcl.Imaging.pngimage;

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

  TGameObject = class;

  TCollider = class
    GameObject: TGameObject;
  end;

  TRigidbody = class
    Gravity: Boolean;
    Velocity: TVector;
  end;

  TGameObject = class
  strict private
    FRigidbody: TRigidbody;
    FCollider: TCollider;
  public
    Position: TPointF;
    destructor Destroy; override;
    property Rigidbody: TRigidbody read FRigidbody;
    procedure AddRigidbody(AGravity: Boolean = True);
    property Collider: TCollider read FCollider;
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
    FSprites: TList;
    FGravity: TPointF;
    function GetAsset(const &Name: string): TAsset;
  public
    constructor Create;
    destructor Destroy; override;
    function CreateAsset(const AResource: string; ASize: TSize; ACount: Integer = 1): TAsset;
    function CreateFont(const AResource: string): TFont;
    function CreateSprite<T: TSprite, constructor>(AParent: TSprite = nil): T;
    procedure UpdateScene;
    procedure DrawScene(ACanvas: TCanvas);
    property Gravity: TPointF read FGravity;
    property Asset[const &Name: string]: TAsset read GetAsset;
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

destructor TGameObject.Destroy;
begin
  if FRigidbody <> nil then
    FRigidbody.Free;
  if FCollider <> nil then
    FCollider.Free;
  inherited;
end;

procedure TGameObject.AddRigidbody(AGravity: Boolean = True);
begin
  FRigidbody := TRigidbody.Create;
  FRigidbody.Gravity := AGravity;
end;

procedure TGameObject.AddCollider(ACollider: TCollider);
begin
  FCollider := ACollider;
  FCollider.GameObject := Self;
end;

{ TSprite }

procedure TSprite.Draw(ACanvas: TCanvas);
const
  C_BLENDFUNCTION: TBlendFunction = (BlendOp: AC_SRC_OVER; SourceConstantAlpha: 255; AlphaFormat: AC_SRC_ALPHA);
begin
  AlphaBlend(ACanvas.Handle, Round(Position.X), Round(Position.Y), Asset.Width, Asset.Height, Asset.Handle, Asset.Offset[CurrentFrame], 0, Asset.Width, Asset.Height, C_BLENDFUNCTION);
end;

{ TGameEngine }

constructor TGameEngine.Create;
begin
  FAssets := TStringList.Create(True);
  FPrivateFonts := TList.Create;
  FSprites := TList.Create;
  FGravity.Y := 1;
end;

destructor TGameEngine.Destroy;
var
  LPointer: Pointer;
begin
  for LPointer in FSprites do
    TSprite(LPointer).Free;
  FSprites.Free;
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

function TGameEngine.CreateSprite<T>(AParent: TSprite = nil): T;
begin
  Result := T.Create;
  if Assigned(AParent) then
    Result.Asset := AParent.Asset;
  FSprites.Add(Pointer(Result));
end;

procedure TGameEngine.UpdateScene;
var
  LSprite: Pointer;
begin
  for LSprite in FSprites do
    if Assigned(TSprite(LSprite).Rigidbody) and TSprite(LSprite).Rigidbody.Gravity then
      TSprite(LSprite).Rigidbody.Velocity.Offset(FGravity);
end;

procedure TGameEngine.DrawScene(ACanvas: TCanvas);
var
  LSprite: Pointer;
begin
  for LSprite in FSprites do
    TSprite(LSprite).Draw(ACanvas);
end;

initialization

GameEngine1 := TGameEngine.Create;

finalization

GameEngine1.Free;

end.
