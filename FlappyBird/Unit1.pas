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
    procedure Animate;
  end;

  TBird = class(TSprite)
    Animation: TAnimation;
    IsDead: Boolean;
  end;

  TCircleCollider = class(TCollider)
    Radius: Single;
  end;

  TSegment = record
    Active: Boolean; // !!

    Position: TPointF;
    Vector: TVector;
    LeftNormal: TVector;
  end;

  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormPaint(Sender: TObject);
  private
    FUpForce: TPointF;
    FBackground, FBackground2: TSprite;
    FGround, FGround2: TSprite;
    FGroundCollider: TSegment;
    FSky: TSegment;
    FBird: TBird;
    FColumns: array [0 .. 5] of TSprite;
    FColumnColliders: array [0 .. 2] of TSegment;
    FScoreColliders: array [0 .. 2] of TSegment;
    FScore: Integer;
    procedure OnIdleHandler(Sender: TObject; var Done: Boolean);
    procedure InitializeGameObjects;
    procedure UpdateScene;
    procedure Restart;
    procedure WMEraseBkgnd(var WMsg: TWMEraseBkgnd); message WM_ERASEBKGND;
  end;

const
  C_ANIM_BIRD_IDLE = 0;
  C_ANIM_BIRD_FLAP = 1;
  C_ANIM_BIRD_DEAD = 2;

var
  Form1: TForm1;

implementation

{$R *.dfm}
// {$DEFINE DEBUGVIEW}

function GetLeftNormal(AVector: TVector): TVector;
begin
  Result.X := AVector.Y;
  Result.Y := -AVector.X;
end;

function CreateSegment(X1, Y1, X2, Y2: Single): TSegment;
begin
  Result.Active := True;
  Result.Position.X := X1;
  Result.Position.Y := Y1;
  Result.Vector.X := X2 - X1;
  Result.Vector.Y := Y2 - Y1;
  Result.LeftNormal := GetLeftNormal(Result.Vector).Normalize;
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
  FBackground.AddRigidbody(False);
  { SKY 2 }
  FBackground2 := GameEngine1.CreateSprite<TSprite>(FBackground);
  { COLUMNS }
  LColumn := GameEngine1.Asset['column'];
  for I := 0 to 5 do
  begin
    FColumns[I] := GameEngine1.CreateSprite<TSprite>;
    FColumns[I].Asset := LColumn;
  end;
  { BIRD }
  FBird := GameEngine1.CreateSprite<TBird>;
  FBird.Asset := GameEngine1.Asset['bird'];
  FBird.AddRigidbody;
  LCircleCollider := TCircleCollider.Create;
  LCircleCollider.Radius := FBird.Asset.Height / 2;
  FBird.AddCollider(LCircleCollider);
  FBird.Animation := TAnimation.Create;
  FBird.Animation.FSprite := FBird;
  { GROUND }
  FGround := GameEngine1.CreateSprite<TSprite>;
  FGround.Asset := GameEngine1.Asset['ground'];
  { GROUND 2 }
  FGround2 := GameEngine1.CreateSprite<TSprite>(FGround);
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  ClientWidth := 1024;
  ClientHeight := 576 - 12;
  GameEngine1.CreateFont('LuckiestGuy');
  Canvas.Brush.Style := bsClear;
  Canvas.Font.Name := 'Luckiest Guy';
  Canvas.Font.Quality := fqAntialiased;
  InitializeAssets;
  InitializeGameObjects;
  FUpForce.Y := -10 * GameEngine1.Gravity.Y;
  FSky := CreateSegment(ClientWidth - 100, 0, 100, 0);
  FGroundCollider := CreateSegment(100, 320 + 128 + 4, ClientWidth - 100, 320 + 128 + 4);
  for I := 0 to 2 do
    FScoreColliders[I] := CreateSegment(0, FBackground.Asset.Height - 10, 0, 10);
  for I := 0 to 2 do
    FColumnColliders[I] := CreateSegment(0, 1, 0, 0);
  Restart;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FBird.Animation.Free;
end;

function GetRandomTop(AHeight: Integer): Single;
begin
  Result := Random(AHeight div 2) + (AHeight / 4)
end;

function GetReflectionTop(AColumn: TSprite): Single;
begin
  Result := AColumn.Position.Y - AColumn.Asset.Height - 100;
end;

function GetColumnColliderPositionX(AColumn: TSprite): Single;
begin
  Result := AColumn.Position.X + 45;
end;

procedure SetColumnColliderSegment(var AColumnCollider: TSegment; AColumn: TSprite);
begin
  AColumnCollider.Position.Y := AColumn.Position.Y + AColumn.Asset.Height;
  AColumnCollider.Vector.Y := -AColumn.Asset.Height;
end;

function GetScoreColliderPositionX(AColumn: TSprite): Single;
begin
  Result := AColumn.Position.X + AColumn.Asset.Width;
end;

procedure TForm1.Restart;
var
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
  FBackground.Rigidbody.Velocity.X := -3;
  FBackground2.Position := PointF(FBackground.Asset.Width, 0);
  FGround.Position := PointF(0, 320 - 12);
  FGround2.Position := PointF(FGround.Asset.Width, 320 - 12);
  for I := 0 to 2 do
  begin
    L := FBackground.Asset.Width + (I * (FBackground.Asset.Width + FColumns[I].Asset.Width) / 3);
    FColumns[I].Position := PointF(L, GetRandomTop(FBackground.Asset.Height));
    FColumns[I + 3].Position := PointF(L, GetReflectionTop(FColumns[I]));
    FColumnColliders[I].Position.X := GetColumnColliderPositionX(FColumns[I]);
    SetColumnColliderSegment(FColumnColliders[I], FColumns[I]);
    FScoreColliders[I].Position.X := GetScoreColliderPositionX(FColumns[I]);
    FScoreColliders[I].Active := True;
  end;
  FScore := 0;
  Application.OnIdle := OnIdleHandler;
end;

procedure TForm1.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #27 then
    Close
  else if (Key = #32) or (Key = '8') then
  begin
    if FBird.IsDead then
      Restart;
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

function CircleVsLine(ACircleCollider: TCircleCollider; ALine: TSegment; out APerpLength: Single): Boolean;
var
  LDistance: TVector;
begin
  LDistance.X := ACircleCollider.GameObject.Position.X + ACircleCollider.Radius - ALine.Position.X;
  LDistance.Y := ACircleCollider.GameObject.Position.Y + ACircleCollider.Radius - ALine.Position.Y;
  APerpLength := LDistance.DotProduct(ALine.LeftNormal);
  Result := APerpLength <= ACircleCollider.Radius;
end;

function ResolveCircleVsLine(ACircleCollider: TCircleCollider; ALine: TSegment; APerpLength: Single): TVector;
begin
  Result := ACircleCollider.GameObject.Rigidbody.Velocity.Normalize;
  Result := Result * (ACircleCollider.Radius - APerpLength) / Result.DotProduct(ALine.LeftNormal);
end;

procedure Die(ABird: TBird; AResolution: TPointF);
begin
  ABird.Position.Offset(AResolution);
  ABird.Rigidbody.Velocity.X := 0;
  ABird.Rigidbody.Velocity.Y := 0;
  ABird.IsDead := True;
  ABird.CurrentFrame := C_ANIM_BIRD_DEAD;
  ABird.Animation.Stop;
end;

procedure TForm1.UpdateScene;
var
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
      FColumns[I].Position.Y := GetRandomTop(FBackground.Asset.Height);
      SetColumnColliderSegment(FColumnColliders[I], FColumns[I]);
      FColumns[I + 3].Position.Offset(L, 0);
      FColumns[I + 3].Position.Y := GetReflectionTop(FColumns[I]);
      FScoreColliders[I].Active := True;
    end;
    FColumnColliders[I].Position.X := GetColumnColliderPositionX(FColumns[I]);
    FScoreColliders[I].Position.X := GetScoreColliderPositionX(FColumns[I]);
  end;
  { COLLISION DETECTION }
  if CircleVsLine(TCircleCollider(FBird.Collider), FGroundCollider, LPerpLength) then
  begin
    FBackground.Rigidbody.Velocity.X := 0;
    Die(FBird, ResolveCircleVsLine(TCircleCollider(FBird.Collider), FGroundCollider, LPerpLength).ToPointF);
  end
  else
  begin
    if FBird.IsDead then
    else
      for I := 0 to 2 do
        if CircleVsLine(TCircleCollider(FBird.Collider), FColumnColliders[I], LPerpLength) then
        begin
          FBackground.Rigidbody.Velocity.X := 0;
          Die(FBird, ResolveCircleVsLine(TCircleCollider(FBird.Collider), FColumnColliders[I], LPerpLength).ToPointF);
        end
        else if FScoreColliders[I].Active and CircleVsLine(TCircleCollider(FBird.Collider), FScoreColliders[I], LPerpLength) then
        begin
          FScoreColliders[I].Active := False;
          Inc(FScore);
        end;
    if CircleVsLine(TCircleCollider(FBird.Collider), FSky, LPerpLength) then
    begin
      FBird.Position.Offset(ResolveCircleVsLine(TCircleCollider(FBird.Collider), FSky, LPerpLength).ToPointF);
      FBird.Rigidbody.Velocity.X := 0;
      FBird.Rigidbody.Velocity.Y := 0;
    end;
  end;
  { BIRD ANIMATION }
  if FBird.Animation.FStarted then
    FBird.Animation.Animate;
end;

procedure DrawSegment(ACanvas: TCanvas; ALine: TSegment);
var
  LPoint: TPoint;
  LColor: TColor;
  LVector: TVector;
begin
  LPoint := ALine.Position.Round;
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

procedure TForm1.FormPaint(Sender: TObject);
{$IFDEF DEBUGVIEW}
var
  LRectF: TRectF;
  LCenter: TPoint;
  LColor: TColor;
  LPoint: TPoint;
  LHead: TPoint;
  // LMatrix: XFORM;
  I: Integer;
{$ENDIF}
begin
{$IFDEF DEBUGVIEW}
  // SetGraphicsMode(Canvas.Handle, GM_ADVANCED);
  // GetWorldTransform(Canvas.Handle, LMatrix);
  // LMatrix.eM11 := 0.5;
  // LMatrix.eM22 := 0.5;
  // LMatrix.eDy := ClientHeight / 4;
  // SetWorldTransform(Canvas.Handle, LMatrix);
{$ENDIF}
  GameEngine1.DrawScene(Canvas);
  if FBird.IsDead then
  begin
    DrawText(Canvas, 36, Rect(0, 0, ClientWidth, 100), 'Game Over', [tfSingleLine, tfCenter, tfBottom]);
    DrawText(Canvas, 18, Rect(0, 110, ClientWidth, 150), 'Flap to restart', [tfSingleLine, tfCenter, tfTop]);
  end;
  DrawText(Canvas, 24, Rect(0, ClientHeight - 80, ClientWidth, ClientHeight), Format('Score: %d', [FScore]), [tfSingleLine, tfCenter, tfTop]);
{$IFDEF DEBUGVIEW}
  { DRAW CIRCLE COLLIDER }
  Canvas.Pen.Color := clWhite;
  LRectF.TopLeft := FBird.Position;
  LRectF.Width := TCircleCollider(FBird.Collider).Radius * 2;
  LRectF.Height := TCircleCollider(FBird.Collider).Radius * 2;
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
  { DRAW LINE COLLIDERS }
  DrawSegment(Canvas, FGroundCollider);
  DrawSegment(Canvas, FSky);
  for I := 0 to 2 do
    DrawSegment(Canvas, FScoreColliders[I]);
  for I := 0 to 2 do
    DrawSegment(Canvas, FColumnColliders[I]);
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

procedure TAnimation.Animate;
const
  ANIM_LENGTH = 200;
begin
  // if not FStarted then
  // Exit;
  if GetTickCount < FLastAnimChangeTime + ANIM_LENGTH then
    FSprite.CurrentFrame := C_ANIM_BIRD_FLAP
  else
  begin
    FSprite.CurrentFrame := C_ANIM_BIRD_IDLE;
    FStarted := False;
  end;
end;

end.
