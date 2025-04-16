unit WaterLevelGraph;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, Math, Definitions;

type
  { TWaterLevelGraph }
  TWaterLevelGraph = class(TCustomControl)
  private
    FWaterLevels: array of Double;
    FCurrentIndex: Integer;
    FTargetLevel: Double;
    procedure SetTargetLevel(AValue: Double);
    function GetGraphWidth: Integer;
    function GetGraphHeight: Integer;
    function GetGraphLeft: Integer;
    function GetGraphTop: Integer;
    function GetGraphRight: Integer;
    function GetGraphBottom: Integer;
    function GetMargin: Integer;
    function GetAxisThickness: Integer;
  protected
    procedure Paint; override;
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure AddDataPoint(Level: Double);
    procedure Clear;
    property TargetLevel: Double read FTargetLevel write SetTargetLevel;
  end;

implementation

{ TWaterLevelGraph }

constructor TWaterLevelGraph.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetLength(FWaterLevels, GRAPH_MAX_POINTS);
  FCurrentIndex := 0;
  FTargetLevel := TARGET_POOL_LEVEL;
  DoubleBuffered := True;
  Color := clWhite;
end;

procedure TWaterLevelGraph.SetTargetLevel(AValue: Double);
begin
  if FTargetLevel = AValue then Exit;
  FTargetLevel := AValue;
  Invalidate;
end;

function TWaterLevelGraph.GetGraphWidth: Integer;
begin
  Result := Width - 2 * GetMargin;
end;

function TWaterLevelGraph.GetGraphHeight: Integer;
begin
  Result := Height - 2 * GetMargin;
end;

function TWaterLevelGraph.GetGraphLeft: Integer;
begin
  Result := GetMargin;
end;

function TWaterLevelGraph.GetGraphTop: Integer;
begin
  Result := GetMargin;
end;

function TWaterLevelGraph.GetGraphRight: Integer;
begin
  Result := GetGraphLeft + GetGraphWidth;
end;

function TWaterLevelGraph.GetGraphBottom: Integer;
begin
  Result := GetGraphTop + GetGraphHeight;
end;

function TWaterLevelGraph.GetMargin: Integer;
begin
  Result := Round(Min(Width, Height) * 0.05);
end;

function TWaterLevelGraph.GetAxisThickness: Integer;
begin
  Result := Max(1, Round(Min(Width, Height) * 0.005));
end;

procedure TWaterLevelGraph.AddDataPoint(Level: Double);
begin
  FWaterLevels[FCurrentIndex] := Level;
  FCurrentIndex := (FCurrentIndex + 1) mod GRAPH_MAX_POINTS;
  Invalidate;
end;

procedure TWaterLevelGraph.Clear;
var
  i: Integer;
begin
  for i := 0 to High(FWaterLevels) do
    FWaterLevels[i] := 0;
  FCurrentIndex := 0;
  Invalidate;
end;

procedure TWaterLevelGraph.Paint;
var
  i, LastX, LastY, X, Y: Integer;
  TargetY: Integer;
  FontSize: Integer;
begin
  inherited Paint;

  // Заливаем фон
  Canvas.Brush.Color := GRAPH_BG_COLOR;
  Canvas.FillRect(GetGraphLeft, GetGraphTop, GetGraphRight, GetGraphBottom);

  // Рисуем оси
  Canvas.Pen.Color := clBlack;
  Canvas.Pen.Width := GetAxisThickness;

  // Вертикальная ось с подписями
  Canvas.Line(GetGraphLeft, GetGraphTop, GetGraphLeft, GetGraphBottom);

  FontSize := Max(10, Round(Min(Width, Height) * 0.025));
  Canvas.Font.Size := FontSize;

  Canvas.TextOut(GetGraphLeft + 5, GetGraphTop + 10, '100%');
  Canvas.TextOut(GetGraphLeft + 5, (GetGraphTop + GetGraphBottom) div 2, '50%');
  Canvas.TextOut(GetGraphLeft + 5, GetGraphBottom - 35, '0%');

  // Горизонтальная ось (время)
  Canvas.Line(GetGraphLeft, GetGraphBottom, GetGraphRight, GetGraphBottom);

  // Линия целевого уровня
  TargetY := GetGraphBottom - Round(GetGraphHeight * FTargetLevel / 100);
  Canvas.Pen.Style := psDash;
  Canvas.Pen.Color := TARGET_LINE_COLOR;
  Canvas.Line(GetGraphLeft, TargetY, GetGraphRight, TargetY);
  Canvas.Pen.Style := psSolid;

  Canvas.TextOut(GetGraphLeft + 5, TargetY - 45,
    Format('%.0f%%', [FTargetLevel]));

  // Рисуем график уровня воды
  if FCurrentIndex > 0 then
  begin
    Canvas.Pen.Color := WATER_LINE_COLOR;
    Canvas.Pen.Width := GRAPH_LINE_WIDTH;

    LastX := GetGraphLeft;
    LastY := GetGraphBottom - Round(FWaterLevels[0] * GetGraphHeight / 100);

    for i := 1 to FCurrentIndex - 1 do
    begin
      X := GetGraphLeft + Round(i * GetGraphWidth / GRAPH_MAX_POINTS);
      Y := GetGraphBottom - Round(FWaterLevels[i] * GetGraphHeight / 100);

      Canvas.Line(LastX, LastY, X, Y);

      LastX := X;
      LastY := Y;
    end;
  end;
end;

procedure TWaterLevelGraph.Resize;
begin
  inherited Resize;
  Invalidate;
end;

end.
