unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.Types, System.UITypes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Imaging.pngimage,
  GameEngine;

type
  TAnimation = class
    FSprite: TSprite;
    FLastAnimChangeTime: DWORD;
    FStarted: Boolean;
    procedure Start;
    procedure Stop;
    procedure Animate; virtual; abstract;
  end;

  TDieAnimation = class(TAnimation)
    procedure Animate; override;
  end;

  TBirdAnimation = class(TAnimation)
    procedure Animate; override;
  end;

  TBird = class(TSprite)
    Animation: TBirdAnimation;
    IsDead: Boolean;
  end;

  TCircleCollider = class(TCollider)
    Radius: Single;
  end;

  TLineDirection = (ldOther, ldHorizontal, ldVertical);

  TLineCollider = class(TCollider)
  strict private
    FVector: TVector;
    FLeftNormal: TVector;
  private
    procedure SetVector(const Value: TVector);
  public
    Direction: TLineDirection;
    property Vector: TVector read FVector write SetVector;
    property LeftNormal: TVector read FLeftNormal;
  end;

  TSegmentCollider = class(TLineCollider);

  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormPaint(Sender: TObject);
  private
    FUpForce: TPointF;
    FBackground, FBackground2: TSprite;
    FGround, FGround2: TSprite;
    FGroundCollider: TLineCollider;
    FSkyCollider: TLineCollider;
    FBird: TBird;
    FDieAnimation: TDieAnimation;
    FColumns: array [0 .. 5] of TSprite;
    FColumnColliders: array [0 .. 5] of TSegmentCollider;
    FScoreCollider: array [0 .. 2] of TLineCollider;
    FScore: Integer;
    procedure OnIdleHandler(Sender: TObject; var Done: Boolean);
    procedure InitializeGameObjects;
    procedure UpdateScene;
    procedure CollisionDetectionBroadPhase;
    procedure Restart;
    procedure WMEraseBkgnd(var WMsg: TWMEraseBkgnd); message WM_ERASEBKGND;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}
{$DEFINE DEBUGVIEW}

const
{$IFDEF DEBUGVIEW}
  C_SCALE_FACTOR = 0.75;
{$ENDIF}
  C_ANIM_BIRD_IDLE = 0;
  C_ANIM_BIRD_FLAP = 1;
  C_ANIM_BIRD_DEAD = 2;

  C_DOOR_HEIGHT = 100;

  C_GRIDX_CELLS_COUNT = 5;
  C_GRIDY_CELLS_COUNT = 3;
  C_GRID_CELL_CAPACITY = 16;

type
  TGridCell = array [0 .. C_GRID_CELL_CAPACITY - 1] of TCollider;

var
  GridX: array [0 .. C_GRIDX_CELLS_COUNT - 1] of TGridCell;
  GridXCellWidth: Single;
  GridY: array [0 .. C_GRIDY_CELLS_COUNT - 1] of TGridCell;
  GridYCellHeight: Single;

function GetLeftNormal(AVector: TVector): TVector;
begin
  Result.X := AVector.Y;
  Result.Y := -AVector.X;
  // Result.W:= AVector.W;
end;

procedure InitializeAssets;
begin
  GameEngine1.CreateAsset('sky', TSize.Create(1024, 512));
  GameEngine1.CreateAsset('column', TSize.Create(128, 512));
  GameEngine1.CreateAsset('bird', TSize.Create(50, 50), 3);
  GameEngine1.CreateAsset('ground', TSize.Create(1024, 256))
end;

procedure TForm1.InitializeGameObjects;
var
  LCircleCollider: TCircleCollider;
  LColumn: TAsset;
  I: Integer;
begin
  { SKY }
  FBackground := GameEngine1.CreateSprite<TSprite>;
  FBackground.Asset := GameEngine1.Asset['sky'];
  FBackground.AddRigidbody(0);
  FSkyCollider := TLineCollider.Create;
  FSkyCollider.Direction := ldHorizontal;
  FSkyCollider.Position.X := FBackground.Asset.Width;
  FSkyCollider.Vector := Vector(-FBackground.Asset.Width, 0);
  FBackground.AddCollider(FSkyCollider);
  { SKY 2 }
  FBackground2 := GameEngine1.CreateSprite<TSprite>(FBackground);
  { COLUMNS }
  LColumn := GameEngine1.Asset['column'];
  for I := 0 to 5 do
  begin
    FColumns[I] := GameEngine1.CreateSprite<TSprite>;
    FColumns[I].Asset := LColumn;
  end;
  for I := 0 to 2 do
  begin
    FScoreCollider[I] := TLineCollider.Create;
    FScoreCollider[I].Direction := ldVertical;
    FScoreCollider[I].Position.X := FColumns[I].Asset.Width;
    FScoreCollider[I].Vector := Vector(0, -C_DOOR_HEIGHT);
    FColumns[I].AddCollider(FScoreCollider[I]);
  end;
  for I := 0 to 5 do
  begin
    FColumnColliders[I] := TSegmentCollider.Create;
    FColumnColliders[I].Direction := ldVertical;
    FColumnColliders[I].Position.X := 50;
    FColumnColliders[I].Position.Y := FColumns[I].Asset.Height;
    FColumnColliders[I].Vector := Vector(0, -FColumns[I].Asset.Height);
    FColumns[I].AddCollider(FColumnColliders[I]);
  end;
  { BIRD }
  FBird := GameEngine1.CreateSprite<TBird>;
  FBird.Asset := GameEngine1.Asset['bird'];
  FBird.AddRigidbody(1);
  LCircleCollider := TCircleCollider.Create;
  LCircleCollider.Radius := FBird.Asset.Height / 2;
  FBird.AddCollider(LCircleCollider);
  FBird.Animation := TBirdAnimation.Create;
  FBird.Animation.FSprite := FBird;
  { GROUND }
  FGround := GameEngine1.CreateSprite<TSprite>;
  FGround.Asset := GameEngine1.Asset['ground'];
  FGround.Position.Y := ClientHeight - FGround.Asset.Height;
  FGroundCollider := TLineCollider.Create;
  FGroundCollider.Direction := ldHorizontal;
  FGroundCollider.Position.X := 100;
  FGroundCollider.Position.Y := 146;
  FGroundCollider.Vector := Vector(ClientWidth - 100, 0);
  FGround.AddCollider(FGroundCollider);
  { GROUND 2 }
  FGround2 := GameEngine1.CreateSprite<TSprite>(FGround);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ClientWidth := 1024;
  ClientHeight := 576 - 12;
  GameEngine1.CreateFont('LuckiestGuy');
  Canvas.Brush.Style := bsClear;
  Canvas.Font.Name := 'Luckiest Guy';
  Canvas.Font.Quality := fqAntialiased;
  InitializeAssets;
  InitializeGameObjects;
  FDieAnimation := TDieAnimation.Create;
  FUpForce.Y := -10 * GameEngine1.Gravity.Y;
  Restart;
  GridXCellWidth := ClientWidth / Length(GridX);
  GridYCellHeight := (FGround.Position.Y + FGroundCollider.Position.Y + 1) / Length(GridY);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FDieAnimation.Free;
  FBird.Animation.Free;
end;

function GetRandomTop(AHeight: Integer): Single;
begin
  Result := Random(AHeight div 2) + (AHeight / 4)
end;

function GetReflectionTop(AColumn: TSprite): Single;
begin
  Result := AColumn.Position.Y - AColumn.Asset.Height - C_DOOR_HEIGHT;
end;

procedure TForm1.Restart;
var
  LCollider: Pointer;
  L: Single;
  I: Integer;
begin
  Application.OnIdle := nil;
  FBird.IsDead := False;
  FBird.CurrentFrame := C_ANIM_BIRD_IDLE;
  FBird.Position.X := (ClientWidth - FBird.Asset.Height) / 2;
  FBird.Position.Y := ClientHeight / 2 - FBird.Asset.Height / 2;
  FBird.Rigidbody.Velocity.X := FUpForce.X;
  FBird.Rigidbody.Velocity.Y := FUpForce.Y;
  FBackground.Position := PointF(0, 0);
  FBackground.Rigidbody.Velocity.X := -4;
  FBackground2.Position := PointF(FBackground.Asset.Width, 0);
  FGround.Position.X := 0;
  FGround2.Position := PointF(FGround.Asset.Width, FGround.Position.Y);
  for I := 0 to 2 do
  begin
    L := FBackground.Asset.Width + (I * (FBackground.Asset.Width + FColumns[I].Asset.Width) / 3);
    FColumns[I].Position := PointF(L, GetRandomTop(FBackground.Asset.Height));
    FColumns[I + 3].Position := PointF(L, GetReflectionTop(FColumns[I]));
  end;
  for I := 0 to 5 do
    for LCollider in FColumns[I].Colliders do
      TCollider(LCollider).Active := True;
  FScore := 0;
  GameEngine1.IsGameOver := False;
  Application.OnIdle := OnIdleHandler;
end;

procedure TForm1.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #27 then
    Close;
  if GameEngine1.IsGameOver then
    Restart;
  if FBird.IsDead then
    Exit;
  if (Key = #32) or (Key = '8') then
  begin
    FBird.Rigidbody.Velocity.X := FUpForce.X;
    FBird.Rigidbody.Velocity.Y := FUpForce.Y;
  end
  else if Key = '6' then
  begin
    FBird.Rigidbody.Velocity.X := 5;
    FBird.Rigidbody.Velocity.Y := FUpForce.Y;
  end
  else if Key = '4' then
  begin
    FBird.Rigidbody.Velocity.X := -5;
    FBird.Rigidbody.Velocity.Y := FUpForce.Y;
  end;
  FBird.Animation.Start;
end;

procedure TForm1.OnIdleHandler(Sender: TObject; var Done: Boolean);
const
  LOOP_TIME = 33;
var
  LStartTime: DWORD;
begin
  LStartTime := GetTickCount;

  UpdateScene;
  Invalidate;

  while GetTickCount - LStartTime < LOOP_TIME do;
end;

function CircleVsLine(ADistance, ALeftNormal: TVector; ARadius: Single; out APerpLength: Single): Boolean; overload;
begin
  APerpLength := ADistance.DotProduct(ALeftNormal);
  Result := APerpLength <= ARadius;
end;

function CircleVsLine(ACircleCollider: TCircleCollider; ALine: TLineCollider; out APerpLength: Single): Boolean; overload;
var
  LDistance: TVector;
begin
  LDistance.X := ACircleCollider.GameObject.Position.X + ACircleCollider.Radius - (ALine.GameObject.Position.X + ALine.Position.X);
  LDistance.Y := ACircleCollider.GameObject.Position.Y + ACircleCollider.Radius - (ALine.GameObject.Position.Y + ALine.Position.Y);
  Result := CircleVsLine(LDistance, ALine.LeftNormal, ACircleCollider.Radius, APerpLength);
end;

function CircleVsSegment(ACircleCollider: TCircleCollider; ASegment: TSegmentCollider; out APerpLength: Single): Boolean; overload;
var
  LDistance: TVector;
  LDotProduct1, LDotProduct2: Single;
begin
  Exit(False);
  if CircleVsLine(ACircleCollider, ASegment, APerpLength) then
  begin
    LDistance.X := ACircleCollider.GameObject.Position.X + ACircleCollider.Radius - (ASegment.GameObject.Position.X + ASegment.Position.X);
    LDistance.Y := ACircleCollider.GameObject.Position.Y + ACircleCollider.Radius - (ASegment.GameObject.Position.Y + ASegment.Position.Y);
    LDotProduct1 := LDistance.DotProduct(ASegment.Vector);
    LDistance.X := ACircleCollider.GameObject.Position.X + ACircleCollider.Radius - (ASegment.GameObject.Position.X + ASegment.Position.X + ASegment.Vector.X);
    LDistance.Y := ACircleCollider.GameObject.Position.Y + ACircleCollider.Radius - (ASegment.GameObject.Position.Y + ASegment.Position.Y + ASegment.Vector.Y);
    LDotProduct2 := LDistance.DotProduct(ASegment.Vector);
    Result := ((LDotProduct1 > 0) and (LDotProduct2 < 0)) or ((LDotProduct1 < 0) and (LDotProduct2 > 0));
  end
  else
    Result := False;
end;

function ResolveCircleVsLine(ACircleCollider: TCircleCollider; ALine: TLineCollider; APerpLength: Single): TVector; overload;
begin
  Result := ACircleCollider.GameObject.Rigidbody.Velocity.Normalize;
  Result := Result * (ACircleCollider.Radius - APerpLength) / Result.DotProduct(ALine.LeftNormal);
end;

procedure Die(ADieAnimation: TDieAnimation; ABird: TBird; AResolution: TPointF);
begin
  ABird.Position.Offset(AResolution);
  ABird.Rigidbody.Velocity.X := 0;
  ABird.Rigidbody.Velocity.Y := 0;
  if ABird.IsDead then
    Exit;
  ABird.IsDead := True;
  ABird.CurrentFrame := C_ANIM_BIRD_DEAD;
  ABird.Animation.Stop;
  ADieAnimation.Start;
end;

procedure TForm1.CollisionDetectionBroadPhase;
var
  LGameObject: Pointer;
  LCollider: Pointer;
  LCircleCollider: TCircleCollider;
  LPointF: TPointF;
  I, J: Integer;
  CX: array [0 .. C_GRIDX_CELLS_COUNT - 1] of Integer;
  CY: array [0 .. C_GRIDY_CELLS_COUNT - 1] of Integer;
  X, Y: Single;
begin
  ZeroMemory(@CX, SizeOf(CX));
  ZeroMemory(@CY, SizeOf(CY));
  ZeroMemory(@GridX, SizeOf(GridX));
  ZeroMemory(@GridY, SizeOf(GridY));
  for LGameObject in GameEngine1.GameObjects do
    for LCollider in TGameObject(LGameObject).Colliders do
      if not TCollider(LCollider).Active then
        Continue
      else if (TCollider(LCollider) is TLineCollider) then
        case TLineCollider(LCollider).Direction of
          ldVertical:
            begin
              X := TCollider(LCollider).GameObject.Position.X + TCollider(LCollider).Position.X;
              if (X < 0) or (X > ClientWidth) then
                Continue;
              I := Trunc(X / GridXCellWidth);
              GridX[I][CX[I]] := LCollider;
              Inc(CX[I]);
            end;
          ldHorizontal:
            begin
              Y := TCollider(LCollider).GameObject.Position.Y + TCollider(LCollider).Position.Y;
              if (Y < 0) or (Y > ClientHeight) then
                Continue;
              J := Trunc(Y / GridYCellHeight);
              GridY[J][CY[J]] := LCollider;
              Inc(CY[J]);
            end;
        end
      else if (TCollider(LCollider) is TCircleCollider) then
      begin
        LCircleCollider := TCircleCollider(LCollider);
        LPointF := LCircleCollider.GameObject.Position + LCircleCollider.Position;
        LPointF.Offset(LCircleCollider.Radius, LCircleCollider.Radius);
        I := Trunc(LPointF.X / GridXCellWidth);
        J := Trunc(LPointF.Y / GridYCellHeight);
        GridX[I][CX[I]] := LCollider;
        GridY[J][CY[J]] := LCollider;
        Inc(CX[I]);
        Inc(CY[J]);
      end;
end;

// procedure TForm1.CollisionDetectionBroadPhase;
// var
// LGameObject: Pointer;
// LCollider: TCollider;
// I, J, C: Integer;
// X: Single;
// begin
// C := 0;
// ZeroMemory(@GridX, SizeOf(GridX));
// for LGameObject in GameEngine1.GameObjects do
// begin
// LCollider := TGameObject(LGameObject).Collider;
// if Assigned(LCollider) then
// begin
// X := LCollider.GameObject.Position.X + LCollider.Position.X;
// if X < 0 then
// Continue;
// if X > ClientWidth then
// Continue;
// GridX[0][C] := LCollider;
// Inc(C);
// end;
// end;
// X := GridXCellWidth;
// I := 0;
// while I < Pred(Length(GridX)) do
// begin
// C := 0;
// for J := 0 to Pred(C_GRIDX_CELL_CAPACITY) do
// begin
// if not Assigned(GridX[I][J]) then
// Break;
// if GridX[I][J].GameObject.Position.X + GridX[I][J].Position.X > X then
// begin
// GridX[Succ(I)][C] := GridX[I][J];
// GridX[I][J] := nil;
// Inc(C);
// end;
// end;
// X := X + GridXCellWidth;
// Inc(I);
// end;
// end;

procedure TForm1.UpdateScene;
var
  LBirdCollider: TCircleCollider;
  LPerpLength: Single;
  L: Single;
  I: Integer;
begin
  GameEngine1.UpdateScene;
  FBird.Position.Offset(FBird.Rigidbody.Velocity.ToPointF);
  FBackground.Position.Offset(FBackground.Rigidbody.Velocity.ToPointF);
  FBackground2.Position.Offset(FBackground.Rigidbody.Velocity.ToPointF);
  FGround.Position.Offset(FBackground.Rigidbody.Velocity.ToPointF);
  FGround2.Position.Offset(FBackground.Rigidbody.Velocity.ToPointF);
  if FBackground.Position.X < -FBackground.Asset.Width then
  begin
    FBackground.Position.Offset(FBackground.Asset.Width * 2, 0);
    FGround.Position.Offset(FGround.Asset.Width * 2, 0);
  end
  else if FBackground2.Position.X < -FBackground.Asset.Width then
  begin
    FBackground2.Position.Offset(FBackground.Asset.Width * 2, 0);
    FGround2.Position.Offset(FGround.Asset.Width * 2, 0);
  end;
  for I := 0 to 2 do
  begin
    FColumns[I].Position.Offset(FBackground.Rigidbody.Velocity.ToPointF);
    FColumns[I + 3].Position.Offset(FBackground.Rigidbody.Velocity.ToPointF);
    if FColumns[I].Position.X < -FColumns[I].Asset.Width then
    begin
      L := FBackground.Asset.Width + FColumns[I].Asset.Width;
      FColumns[I].Position.Offset(L, 0);
      FColumns[I + 3].Position.Offset(L, 0);
      FColumns[I].Position.Y := GetRandomTop(FBackground.Asset.Height);
      FColumns[I + 3].Position.Y := GetReflectionTop(FColumns[I]);
      FColumnColliders[I].Active := True;
      FColumnColliders[I + 3].Active := True;
      FScoreCollider[I].Active := True;
    end;
  end;
  { COLLISION DETECTION BROAD PHASE }
  CollisionDetectionBroadPhase;
  { COLLISION DETECTION }
  LBirdCollider := TCircleCollider(FBird.Colliders[0]);
  if CircleVsLine(LBirdCollider, FGroundCollider, LPerpLength) then
  begin
    FBackground.Rigidbody.Velocity.X := 0;
    Die(FDieAnimation, FBird, ResolveCircleVsLine(LBirdCollider, FGroundCollider, LPerpLength).ToPointF);
  end
  else
  begin
    if not FBird.IsDead then
      for I := 0 to 2 do
        if FColumnColliders[I].Active and CircleVsSegment(LBirdCollider, FColumnColliders[I], LPerpLength) then
        begin
          FBackground.Rigidbody.Velocity.X := 0;
          Die(FDieAnimation, FBird, ResolveCircleVsLine(LBirdCollider, FColumnColliders[I], LPerpLength).ToPointF);
        end
        else if FColumnColliders[I + 3].Active and CircleVsSegment(LBirdCollider, FColumnColliders[I + 3], LPerpLength) then
        begin
          FBackground.Rigidbody.Velocity.X := 0;
          Die(FDieAnimation, FBird, ResolveCircleVsLine(LBirdCollider, FColumnColliders[I + 3], LPerpLength).ToPointF);
        end
        else if FScoreCollider[I].Active and CircleVsLine(LBirdCollider, FScoreCollider[I], LPerpLength) then
        begin
          FColumnColliders[I].Active := False;
          FColumnColliders[I + 3].Active := False;
          FScoreCollider[I].Active := False;
          Inc(FScore);
        end;
    if CircleVsLine(LBirdCollider, FSkyCollider, LPerpLength) then
    begin
      FBird.Position.Offset(ResolveCircleVsLine(LBirdCollider, FSkyCollider, LPerpLength).ToPointF);
      FBird.Rigidbody.Velocity.X := 0;
      FBird.Rigidbody.Velocity.Y := 0;
    end;
  end;
  { BIRD ANIMATION }
  if FBird.Animation.FStarted then
    FBird.Animation.Animate;
  { DIE ANIMATION }
  if FDieAnimation.FStarted then
    FDieAnimation.Animate;
end;

procedure DrawLineCollider(ACanvas: TCanvas; ALine: TLineCollider);
var
  LPoint: TPoint;
  LColor: TColor;
  LVector: TVector;
begin
  LPoint := (ALine.GameObject.Position + ALine.Position).Round;
  ACanvas.MoveTo(LPoint.X, LPoint.Y);
  if not ALine.Active then
    ACanvas.Pen.Style := psDot;
  ACanvas.LineTo(LPoint.X + Round(ALine.Vector.X), LPoint.Y + Round(ALine.Vector.Y));
  ACanvas.Pen.Style := psSolid;
  { DRAW LEFT NORMAL }
  ACanvas.MoveTo(LPoint.X, LPoint.Y);
  LColor := ACanvas.Pen.Color;
  ACanvas.Pen.Color := clRed;
  ACanvas.LineTo(LPoint.X + Round(ALine.LeftNormal.X * 100), LPoint.Y + Round(ALine.LeftNormal.Y * 100));
  ACanvas.MoveTo(LPoint.X + Round(ALine.LeftNormal.X * 10), LPoint.Y + Round(ALine.LeftNormal.Y * 10));
  LVector := ALine.Vector.Normalize;
  ACanvas.LineTo(LPoint.X + Round(ALine.LeftNormal.X * 10) + Round(LVector.X * 10), LPoint.Y + Round(ALine.LeftNormal.Y * 10) + Round(LVector.Y * 10));
  ACanvas.LineTo(LPoint.X + Round(LVector.X * 10), LPoint.Y + Round(LVector.Y * 10));
  ACanvas.Pen.Color := LColor;
end;

procedure DrawText(ACanvas: TCanvas; ASize: Integer; ARect: TRect; AText: string; ATextFormat: TTextFormat);
begin
  ACanvas.Font.Size := ASize;
  ARect.Offset(2, 2);
  ACanvas.Font.Color := clBlack;
  ACanvas.TextRect(ARect, AText, ATextFormat);
  ARect.Offset(-2, -2);
  ACanvas.Font.Color := clWhite;
  ACanvas.TextRect(ARect, AText, ATextFormat);
end;

procedure DrawGridX(ACanvas: TCanvas; AIndex, AHeight: Integer);
var
  X: Integer;
  LRectF: TRectF;
  LColor: TColor;
  I, C: Integer;
begin
  X := Round(Succ(AIndex) * GridXCellWidth);
  ACanvas.MoveTo(X, 0);
  LColor := ACanvas.Pen.Color;
  ACanvas.Pen.Color := clLtGray;
  ACanvas.Pen.Style := psDashDot;
  ACanvas.LineTo(X, AHeight);
  ACanvas.Pen.Color := LColor;
  ACanvas.Pen.Style := psSolid;
  C := 0;
  for I := 0 to Pred(Length(GridX[AIndex])) do
    if Assigned(GridX[AIndex][I]) then
      Inc(C);
  LRectF := RectF(0, AHeight - 100, GridXCellWidth, AHeight - 10);
  LRectF.Offset(AIndex * GridXCellWidth, 0);
  DrawText(ACanvas, 12, LRectF.Round, IntToStr(C), [tfSingleLine, tfCenter, tfBottom]);
end;

procedure DrawGridY(ACanvas: TCanvas; AIndex, AWidth: Integer);
var
  Y: Integer;
  LRectF: TRectF;
  LColor: TColor;
  I, C: Integer;
begin
  Y := Round(Succ(AIndex) * GridYCellHeight);
  ACanvas.MoveTo(0, Y);
  LColor := ACanvas.Pen.Color;
  ACanvas.Pen.Color := clLtGray;
  ACanvas.Pen.Style := psDashDot;
  ACanvas.LineTo(AWidth, Y);
  ACanvas.Pen.Color := LColor;
  ACanvas.Pen.Style := psSolid;
  C := 0;
  for I := 0 to Pred(Length(GridY[AIndex])) do
    if Assigned(GridY[AIndex][I]) then
      Inc(C);
  LRectF := RectF(10, 0, 100, GridYCellHeight);
  LRectF.Offset(0, AIndex * GridYCellHeight);
  DrawText(ACanvas, 12, LRectF.Round, IntToStr(C), [tfSingleLine, tfLeft, tfVerticalCenter]);
end;

procedure TForm1.FormPaint(Sender: TObject);
{$IFDEF DEBUGVIEW}
var
  LGameObject: Pointer;
  LCollider: Pointer;
  LRectF: TRectF;
  LCenter: TPoint;
  LColor: TColor;
  LPoint: TPoint;
  LHead: TPoint;
  LMatrix: XFORM;
  I: Integer;
{$ENDIF}
begin
{$IFDEF DEBUGVIEW}
  SetGraphicsMode(Canvas.Handle, GM_ADVANCED);
  GetWorldTransform(Canvas.Handle, LMatrix);
  LMatrix.eM11 := C_SCALE_FACTOR;
  LMatrix.eM22 := C_SCALE_FACTOR;
  LMatrix.eDy := ClientHeight * (1 - C_SCALE_FACTOR) / 2;
  SetWorldTransform(Canvas.Handle, LMatrix);
{$ENDIF}
  GameEngine1.DrawScene(Canvas);
  if FBird.IsDead then
    DrawText(Canvas, 36, Rect(0, 0, ClientWidth, 100), 'Game Over', [tfSingleLine, tfCenter, tfBottom]);
  if GameEngine1.IsGameOver then
    DrawText(Canvas, 18, Rect(0, 110, ClientWidth, 150), 'Flap to restart', [tfSingleLine, tfCenter, tfTop]);
  DrawText(Canvas, 24, Rect(0, ClientHeight - 80, ClientWidth, ClientHeight), Format('Score: %d', [FScore]), [tfSingleLine, tfCenter, tfTop]);
{$IFDEF DEBUGVIEW}
  { DRAW CIRCLE COLLIDER }
  Canvas.Pen.Color := clWhite;
  LRectF.TopLeft := FBird.Position;
  LRectF.Width := TCircleCollider(FBird.Colliders[0]).Radius * 2;
  LRectF.Height := TCircleCollider(FBird.Colliders[0]).Radius * 2;
  Canvas.Ellipse(LRectF.Round);
  { DRAW VELOCITY }
  LCenter := LRectF.CenterPoint.Round;
  Canvas.MoveTo(LCenter.X, LCenter.Y);
  LColor := Canvas.Pen.Color;
  Canvas.Pen.Color := clLime;
  LPoint := FBird.Rigidbody.Velocity.ToPointF.Round;
  LPoint := Point(LPoint.X shl 2, LPoint.Y shl 2);
  LPoint.Offset(LCenter);
  Canvas.LineTo(LPoint.X, LPoint.Y);
  { DRAW HEAD }
  LHead := FBird.Rigidbody.Velocity.Normalize.Rotate(Pi * -0.25).ToPointF.Round;
  LHead := Point(-LHead.X shl 2, -LHead.Y shl 2);
  LHead.Offset(LPoint);
  Canvas.LineTo(LHead.X, LHead.Y);
  Canvas.MoveTo(LPoint.X, LPoint.Y);
  LHead := FBird.Rigidbody.Velocity.Normalize.Rotate(Pi * 0.25).ToPointF.Round;
  LHead := Point(-LHead.X shl 2, -LHead.Y shl 2);
  LHead.Offset(LPoint);
  Canvas.LineTo(LHead.X, LHead.Y);
  Canvas.Pen.Color := LColor;
  { DRAW LINE/SEGMENT COLLIDERS }
  for LGameObject in GameEngine1.GameObjects do
    for LCollider in TGameObject(LGameObject).Colliders do
      if TCollider(LCollider) is TLineCollider then
        DrawLineCollider(Canvas, TLineCollider(LCollider));
  { DRAW X GRID }
  for I := 0 to Pred(Length(GridX)) do
    DrawGridX(Canvas, I, ClientHeight);
  { DRAW Y GRID }
  for I := 0 to Pred(Length(GridY)) do
    DrawGridY(Canvas, I, ClientWidth);
{$ENDIF}
end;

procedure TForm1.WMEraseBkgnd(var WMsg: TWMEraseBkgnd);
begin
{$IFDEF DEBUGVIEW}
  inherited;
{$ELSE}
  WMsg.Result := 1;
{$ENDIF}
end;

{ TLineCollider }

procedure TLineCollider.SetVector(const Value: TVector);
begin
  FVector := Value;
  FLeftNormal := GetLeftNormal(Value).Normalize;
end;

{ TAnimation }

procedure TAnimation.Start;
begin
  FLastAnimChangeTime := GetTickCount;
  FStarted := True;
end;

procedure TAnimation.Stop;
begin
  FStarted := False;
end;

{ TBirdAnimation }

procedure TBirdAnimation.Animate;
const
  ANIM_LENGTH = 200;
begin
  if GetTickCount < FLastAnimChangeTime + ANIM_LENGTH then
    FSprite.CurrentFrame := C_ANIM_BIRD_FLAP
  else
  begin
    FSprite.CurrentFrame := C_ANIM_BIRD_IDLE;
    FStarted := False;
  end;
end;

{ TDieAnimation }

procedure TDieAnimation.Animate;
const
  ANIM_LENGTH = 2000;
begin
  if GetTickCount < FLastAnimChangeTime + ANIM_LENGTH then
  else
  begin
    GameEngine1.IsGameOver := True;
    FStarted := False;
  end;
end;

end.
