unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.Types, System.UITypes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Imaging.pngimage,
  GameEngine, CollisionsDetection;

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

  TBackground = class(TSprite)
  protected
    procedure Update; override;
  end;

  TColumn = class(TSprite)
  protected
    procedure Update; override;
  end;

  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormPaint(Sender: TObject);
  private
    FUpForce: TPointF;
    FBackground1, FBackground2: TBackground;
    FGround: TSprite;
    FGroundCollider: THorzLineCollider;
    FSkyCollider: THorzLineCollider;
    FBird: TBird;
    FDieAnimation: TDieAnimation;
    FColumns: array [0 .. 5] of TColumn;
    FColumnColliders: array [0 .. 5] of TVertSegmentCollider;
    FScoreCollider: array [0 .. 2] of TVertLineCollider;
    FScore: Integer;
    procedure OnIdleHandler(Sender: TObject; var Done: Boolean);
    procedure InitializeGameObjects;
    procedure UpdateScene;
    procedure Stop;
    procedure Restart;
    procedure WMEraseBkgnd(var WMsg: TWMEraseBkgnd); message WM_ERASEBKGND;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}
// {$DEFINE DEBUGVIEW}

const
{$IFDEF DEBUGVIEW}
  C_SCALE_FACTOR = 0.75;
{$ENDIF}
  C_ANIM_BIRD_IDLE = 0;
  C_ANIM_BIRD_FLAP = 1;
  C_ANIM_BIRD_DEAD = 2;

  C_BKGND_SPEED = 4;
{$IFDEF DEBUGVIEW}
  C_DOOR_HEIGHT = 130;
{$ELSE}
  C_DOOR_HEIGHT = 70;
{$ENDIF}

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
  LGameObject: TGameObject;
  I: Integer;
begin
  { SKY }
  FBackground1 := GameEngine1.CreateSprite<TBackground>;
  FBackground1.Asset := GameEngine1.Asset['sky'];
  FBackground1.AddRigidbody(0);
  FSkyCollider := THorzLineCollider.Create;
  FSkyCollider.Position.X := FBackground1.Asset.Width - 100;
  FSkyCollider.Vector := Vector(200 - FBackground1.Asset.Width, 0);
  FSkyCollider.CalcLeftNormal;
  { SKY 2 }
  FBackground2 := GameEngine1.CreateSprite<TBackground>;
  FBackground2.Asset := FBackground1.Asset;
  FBackground2.AddRigidbody(0);
  { COLUMNS }
  LColumn := GameEngine1.Asset['column'];
  for I := 0 to 2 do
  begin
    FColumns[I] := GameEngine1.CreateSprite<TColumn>;
    FColumns[I].Asset := LColumn;
    FColumns[I].AddRigidbody(0);
  end;
  for I := 3 to 5 do
  begin
    FColumns[I] := GameEngine1.CreateSprite<TColumn>(FColumns[I - 3]);
    FColumns[I].Asset := LColumn;
    FColumns[I].Position.Y := -LColumn.Height - C_DOOR_HEIGHT;
  end;
  for I := 0 to 2 do
  begin
    FScoreCollider[I] := TVertLineCollider.Create;
    FScoreCollider[I].Position.X := LColumn.Width;
    FScoreCollider[I].Vector := Vector(0, -C_DOOR_HEIGHT);
    FScoreCollider[I].CalcLeftNormal;
    FColumns[I].AddCollider(FScoreCollider[I]);
  end;
  for I := 0 to 2 do
  begin
    FColumnColliders[I] := TVertSegmentCollider.Create;
    FColumnColliders[I].Position.X := 50;
    FColumnColliders[I].Position.Y := LColumn.Height;
    FColumnColliders[I].Vector := Vector(0, -LColumn.Height);
    FColumnColliders[I].CalcLeftNormal;
    FColumns[I].AddCollider(FColumnColliders[I]);
  end;
  for I := 3 to 5 do
  begin
    FColumnColliders[I] := TVertSegmentCollider.Create;
    FColumnColliders[I].Position.X := 50;
    FColumnColliders[I].Position.Y := -C_DOOR_HEIGHT;
    FColumnColliders[I].Vector := Vector(0, -LColumn.Height);
    FColumnColliders[I].CalcLeftNormal;
    FColumns[I - 3].AddCollider(FColumnColliders[I]);
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
  FGround := GameEngine1.CreateSprite<TSprite>(FBackground1);
  FGround.Asset := GameEngine1.Asset['ground'];
  FGround.Position.Y := ClientHeight - FGround.Asset.Height;
  { GROUND 2 }
  with GameEngine1.CreateSprite<TSprite>(FBackground2) do
  begin
    Asset := FGround.Asset;
    Position.Y := FGround.Position.Y
  end;
  { SCENE }
  LGameObject := TGameObject.Create;
  GameEngine1.GameObjects.Add(LGameObject);
  LGameObject.AddCollider(FSkyCollider);
  FGroundCollider := THorzLineCollider.Create;
  FGroundCollider.Position.X := 100;
  FGroundCollider.Position.Y := FGround.Position.Y + 146;
  FGroundCollider.Vector := Vector(FGround.Asset.Width - 200, 0);
  FGroundCollider.CalcLeftNormal;
  LGameObject.AddCollider(FGroundCollider);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ClientWidth := 1024;
  ClientHeight := 576 - 12;
  GameEngine1.Width := ClientWidth;
  GameEngine1.Height := ClientHeight;
  GameEngine1.CreateFont('LuckiestGuy');
  Canvas.Brush.Style := bsClear;
  Canvas.Font.Name := 'Luckiest Guy';
  Canvas.Font.Quality := fqAntialiased;
  InitializeAssets;
  InitializeGameObjects;
  FDieAnimation := TDieAnimation.Create;
  FUpForce.Y := -10 * GameEngine.C_GRAVITY;
  CollisionsDetection.GridXCellWidth := ClientWidth / C_GRIDX_CELLS_COUNT;
  CollisionsDetection.GridYCellHeight := (FBackground1.Position.Y + FGroundCollider.Position.Y + 1) / Length(GridY);
  // InitializeGrids;
  GridYAdd(FSkyCollider);
  GridYAdd(FGroundCollider);
  Restart;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  // FinalizeGrids;
  FDieAnimation.Free;
  FBird.Animation.Free;
end;

function GetRandomTop(AHeight: Integer): Single;
begin
  Result := Random(AHeight div 2) + (AHeight / 4)
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
  FBackground1.Position.X := 0;
  FBackground1.Rigidbody.Velocity.X := -C_BKGND_SPEED;
  FBackground2.Position.X := FBackground1.Asset.Width;
  FBackground2.Rigidbody.Velocity.X := FBackground1.Rigidbody.Velocity.X;
  for I := 0 to 2 do
  begin
    L := FBackground1.Asset.Width + (I * (FBackground1.Asset.Width + FColumns[I].Asset.Width) / 3);
    FColumns[I].Position := PointF(L, GetRandomTop(FBackground1.Asset.Height));
    FColumns[I].Rigidbody.Velocity.X := -C_BKGND_SPEED;
  end;
  { ACTIVATE COLLIDERS }
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
  LDistance.X := TGameObject(ACircleCollider.GameObject).Position.X + ACircleCollider.Radius - (TGameObject(ALine.GameObject).Position.X + ALine.Position.X);
  LDistance.Y := TGameObject(ACircleCollider.GameObject).Position.Y + ACircleCollider.Radius - (TGameObject(ALine.GameObject).Position.Y + ALine.Position.Y);
  Result := CircleVsLine(LDistance, ALine.LeftNormal, ACircleCollider.Radius, APerpLength);
end;

function CircleVsSegment(ACircleCollider: TCircleCollider; ASegment: TSegmentCollider; out APerpLength: Single): Boolean; overload;
var
  LDistance: TVector;
  LDotProduct1, LDotProduct2: Single;
begin
  if CircleVsLine(ACircleCollider, ASegment, APerpLength) then
  begin
    LDistance.X := TGameObject(ACircleCollider.GameObject).Position.X + ACircleCollider.Radius - (TGameObject(ASegment.GameObject).Position.X + ASegment.Position.X);
    LDistance.Y := TGameObject(ACircleCollider.GameObject).Position.Y + ACircleCollider.Radius - (TGameObject(ASegment.GameObject).Position.Y + ASegment.Position.Y);
    LDotProduct1 := LDistance.DotProduct(ASegment.Vector);
    LDistance.X := TGameObject(ACircleCollider.GameObject).Position.X + ACircleCollider.Radius - (TGameObject(ASegment.GameObject).Position.X + ASegment.Position.X + ASegment.Vector.X);
    LDistance.Y := TGameObject(ACircleCollider.GameObject).Position.Y + ACircleCollider.Radius - (TGameObject(ASegment.GameObject).Position.Y + ASegment.Position.Y + ASegment.Vector.Y);
    LDotProduct2 := LDistance.DotProduct(ASegment.Vector);
    Result := ((LDotProduct1 > 0) and (LDotProduct2 < 0)) or ((LDotProduct1 < 0) and (LDotProduct2 > 0));
  end
  else
    Result := False;
end;

function ResolveCircleVsLine(ACircleCollider: TCircleCollider; ALine: TLineCollider; APerpLength: Single): TVector; overload;
begin
  Result := TGameObject(ACircleCollider.GameObject).Rigidbody.Velocity.Normalize;
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

{ TBackground }

procedure TBackground.Update;
begin
  inherited;
  if Position.X < -Asset.Width then
    Position.Offset(Asset.Width * 2, 0);
end;

{ TColumn }

procedure TColumn.Update;
var
  LCollider: Pointer;
  I: Integer;
begin
  inherited;
  if Position.X < -Asset.Width then
  begin
    Position.X := Position.X + GameEngine1.Width + Asset.Width;
    Position.Y := GetRandomTop(GameEngine1.Height);
    for I := 0 to 5 do
      for LCollider in Colliders do
        TCollider(LCollider).Active := True;
  end;
end;

procedure TForm1.UpdateScene;
var
  LBirdCollider: TCircleCollider;
  LCollider: Pointer;
  LPerpLength: Single;
  I: Integer;
begin
  GameEngine1.UpdateScene;
  { BROAD PHASE }
  UpdateGridFor(FBird.Colliders[0]);
  for I := 0 to 5 do
    for LCollider in FColumns[I].Colliders do
      UpdateGridFor(LCollider);
  { COLLISION DETECTION }
  LBirdCollider := TCircleCollider(FBird.Colliders[0]);
  if CircleVsLine(LBirdCollider, FGroundCollider, LPerpLength) then
  begin
    Stop;
    Die(FDieAnimation, FBird, ResolveCircleVsLine(LBirdCollider, FGroundCollider, LPerpLength).ToPointF);
  end
  else
  begin
    if not FBird.IsDead then
      for I := 0 to 2 do
        if FColumnColliders[I].Active and CircleVsSegment(LBirdCollider, FColumnColliders[I], LPerpLength) then
        begin
          Stop;
          Die(FDieAnimation, FBird, ResolveCircleVsLine(LBirdCollider, FColumnColliders[I], LPerpLength).ToPointF);
        end
        else if FColumnColliders[I + 3].Active and CircleVsSegment(LBirdCollider, FColumnColliders[I + 3], LPerpLength) then
        begin
          Stop;
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

procedure TForm1.Stop;
var
  I: Integer;
begin
  FBackground1.Rigidbody.Velocity.X := 0;
  FBackground2.Rigidbody.Velocity.X := 0;
  for I := 0 to 2 do
    FColumns[I].Rigidbody.Velocity.X := 0;
end;

procedure DrawLineCollider(ACanvas: TCanvas; ALine: TLineCollider);
var
  LPoint: TPoint;
  LColor: TColor;
  LVector: TVector;
begin
  LPoint := (TGameObject(ALine.GameObject).Position + ALine.Position).Round;
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
begin
  X := Round(AIndex * GridXCellWidth);
  ACanvas.MoveTo(X, 0);
  LColor := ACanvas.Pen.Color;
  ACanvas.Pen.Color := clLtGray;
  ACanvas.Pen.Style := psDashDot;
  ACanvas.LineTo(X, AHeight);
  ACanvas.Pen.Color := LColor;
  ACanvas.Pen.Style := psSolid;
  LRectF := RectF(0, AHeight - 100, GridXCellWidth, AHeight - 10);
  LRectF.Offset(Pred(AIndex) * GridXCellWidth, 0);
  DrawText(ACanvas, 12, LRectF.Round, IntToStr( { C } GridX[AIndex].Count), [tfSingleLine, tfCenter, tfBottom]);
end;

procedure DrawGridY(ACanvas: TCanvas; AIndex, AWidth: Integer);
var
  Y: Integer;
  LRectF: TRectF;
  LColor: TColor;
begin
  Y := Round(AIndex * GridYCellHeight);
  ACanvas.MoveTo(0, Y);
  LColor := ACanvas.Pen.Color;
  ACanvas.Pen.Color := clLtGray;
  ACanvas.Pen.Style := psDashDot;
  ACanvas.LineTo(AWidth, Y);
  ACanvas.Pen.Color := LColor;
  ACanvas.Pen.Style := psSolid;
  LRectF := RectF(10, 0, 100, GridYCellHeight);
  LRectF.Offset(0, Pred(AIndex) * GridYCellHeight);
  DrawText(ACanvas, 12, LRectF.Round, IntToStr( { C } GridY[AIndex].Count), [tfSingleLine, tfLeft, tfVerticalCenter]);
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
  for I := 1 to Length(GridX) do
    DrawGridX(Canvas, I, ClientHeight);
  { DRAW Y GRID }
  for I := 1 to Length(GridY) do
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
  ANIM_LENGTH = 1500;
begin
  if GetTickCount < FLastAnimChangeTime + ANIM_LENGTH then
  else
  begin
    GameEngine1.IsGameOver := True;
    FStarted := False;
  end;
end;

end.
