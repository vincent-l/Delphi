unit CollisionsDetection;

interface

uses
  System.Classes, System.Types;

type
  // TGameObject = class;

  TCollider = class
    Active: Boolean;
    Position: TPointF; // relative position
    GameObject: Pointer;

    GridXIndex: Integer;
    GridYIndex: Integer;

    constructor Create;
  end;

  TCircleCollider = class(TCollider)
    Radius: Single;
  end;

  TLineCollider = class(TCollider)
  strict private
    FLeftNormal: TVector;
  public
    Vector: TVector;
    procedure CalcLeftNormal;
    property LeftNormal: TVector read FLeftNormal;
  end;

  THorzLineCollider = class(TLineCollider);
  TVertLineCollider = class(TLineCollider);

  TSegmentCollider = class(TLineCollider);

  THorzSegmentCollider = class(TSegmentCollider);
  TVertSegmentCollider = class(TSegmentCollider);

procedure GridXAdd(ACollider: TCollider);
// procedure GridXRemove(ACollider: TCollider);
procedure GridYAdd(ACollider: TCollider);
// procedure GridYRemove(ACollider: TCollider);
procedure UpdateGridFor(ACollider: TCollider);

const
  C_GRIDX_CELLS_COUNT = 5;
  C_GRIDY_CELLS_COUNT = 3;
  // C_GRID_CELL_CAPACITY = 16;

  // type
  // TGridCell = array [1 .. C_GRID_CELL_CAPACITY] of TCollider;

  // TGrid = record
  // Grid: array of TGridCell;
  // CellExtent: Single;
  // end;

var
  GridX: array [1 .. C_GRIDX_CELLS_COUNT] of TList;
  GridXCellWidth: Single;
  GridY: array [1 .. C_GRIDY_CELLS_COUNT] of TList;
  GridYCellHeight: Single;

  // function CreateGrid(ACellsCount: Integer; ACellExtent: Single): TGrid;
  // begin
  // SetLength(Result.Grid, ACellsCount);
  // Result.CellExtent := ACellExtent;
  // end;

implementation

uses
  GameEngine;

procedure InitializeGrids;
var
  I: Integer;
begin
  // GridX := CreateGrid(C_GRIDX_CELLS_COUNT, ClientWidth / C_GRIDX_CELLS_COUNT);
  // GridY := CreateGrid(C_GRIDY_CELLS_COUNT, (FGround.Position.Y + FGroundCollider.Position.Y + 1) / Length(GridY));
  for I := 1 to C_GRIDX_CELLS_COUNT do
    GridX[I] := TList.Create;
  for I := 1 to C_GRIDY_CELLS_COUNT do
    GridY[I] := TList.Create;
end;

procedure FinalizeGrids;
var
  I: Integer;
begin
  for I := 1 to C_GRIDX_CELLS_COUNT do
    GridX[I].Free;
  for I := 1 to C_GRIDY_CELLS_COUNT do
    GridY[I].Free;
end;

procedure GridXAdd(ACollider: TCollider);
var
  X: Single;
begin
  X := TGameObject(ACollider.GameObject).Position.X + ACollider.Position.X;
  if (X < 0) or (X > Length(GridX) * GridXCellWidth) then
  else
  begin
    ACollider.GridXIndex := Succ(Trunc(X / GridXCellWidth));
    GridX[ACollider.GridXIndex].Add(ACollider);
  end;
end;

procedure GridXRemove(ACollider: TCollider);
begin
  if ACollider.GridXIndex = 0 then
    Exit;
  GridX[ACollider.GridXIndex].Remove(ACollider);
  ACollider.GridXIndex := 0;
end;

procedure GridYAdd(ACollider: TCollider);
var
  Y: Single;
begin
  Y := TGameObject(ACollider.GameObject).Position.Y + ACollider.Position.Y;
  if (Y < 0) or (Y > Length(GridY) * GridYCellHeight) then
  else
  begin
    ACollider.GridYIndex := Succ(Trunc(Y / GridYCellHeight));
    GridY[ACollider.GridYIndex].Add(ACollider);
  end;
end;

procedure GridYRemove(ACollider: TCollider);
begin
  if ACollider.GridYIndex = 0 then
    Exit;
  GridY[ACollider.GridYIndex].Remove(ACollider);
  ACollider.GridYIndex := 0;
end;

procedure UpdateGridFor(ACollider: TCollider);
begin
  GridXRemove(ACollider);
  GridYRemove(ACollider);
  if not ACollider.Active then
    Exit;
  if (ACollider is TCircleCollider) then
  begin
    GridXAdd(ACollider);
    GridYAdd(ACollider);
  end
  else if (ACollider is TVertLineCollider) or (ACollider is TVertSegmentCollider) then
    GridXAdd(ACollider)
  else if (ACollider is THorzLineCollider) or (ACollider is THorzSegmentCollider) then
    GridYAdd(ACollider);
end;

{ TCollider }

constructor TCollider.Create;
begin
  Active := True;
end;

{ TLineCollider }

procedure TLineCollider.CalcLeftNormal;
begin
  FLeftNormal.X := Vector.Y;
  FLeftNormal.Y := -Vector.X;
  // Result.W:= AVector.W;
  FLeftNormal := FLeftNormal.Normalize;
end;

initialization

InitializeGrids;

finalization

FinalizeGrids;

end.
