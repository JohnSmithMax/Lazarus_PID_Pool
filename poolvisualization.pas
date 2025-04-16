unit PoolVisualization;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, Definitions, Math, Types;

type
  { TPoolVisualization }
  TPoolVisualization = class(TCustomControl)
  private
    FWaterLevel: Double;
    FInflowRate: Double;
    FOutflowRate: Double;
    procedure SetWaterLevel(AValue: Double);
    procedure SetInflowRate(AValue: Double);
    procedure SetOutflowRate(AValue: Double);
  protected
    procedure Paint; override;
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure UpdateWaterLevel(NewLevel: Double);

    property WaterLevel: Double read FWaterLevel write SetWaterLevel;
    property InflowRate: Double read FInflowRate write SetInflowRate;
    property OutflowRate: Double read FOutflowRate write SetOutflowRate;
  end;

implementation

{ TPoolVisualization }

constructor TPoolVisualization.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FWaterLevel := START_WATER_LEVEL;
  FInflowRate := 0;
  FOutflowRate := 0;
  DoubleBuffered := True;
  Color := clWhite;
end;

procedure TPoolVisualization.SetWaterLevel(AValue: Double);
begin
  if FWaterLevel = AValue then Exit;
  FWaterLevel := AValue;
  Invalidate;
end;

procedure TPoolVisualization.SetInflowRate(AValue: Double);
begin
  if FInflowRate = AValue then Exit;
  FInflowRate := AValue;
  Invalidate;
end;

procedure TPoolVisualization.SetOutflowRate(AValue: Double);
begin
  if FOutflowRate = AValue then Exit;
  FOutflowRate := AValue;
  Invalidate;
end;

procedure TPoolVisualization.UpdateWaterLevel(NewLevel: Double);
begin
  WaterLevel := NewLevel;
end;

procedure TPoolVisualization.Paint;
var
  PoolRect, WaterRect: TRect;
  TargetY, PipeInY, PipeOutY: Integer;
  PipeThickness: Integer;
  TextSize: TSize;
begin
  inherited Paint;

  // Настройка фона
  Canvas.Brush.Color := Color;
  Canvas.FillRect(ClientRect);

  // Размеры бассейна (с отступами 5%)
  PoolRect := Rect(
    Round(Width * 0.05),
    Round(Height * 0.05),
    Round(Width * 0.95),
    Round(Height * 0.95)
  );

  // Рисуем бассейн (контейнер)
  Canvas.Pen.Color := POOL_BG_LINE_COLOR;
  Canvas.Pen.Width := 2;
  Canvas.Brush.Color := POOL_BG_COLOR;
  Canvas.Rectangle(PoolRect);

  // Рисуем воду
  WaterRect := PoolRect;
  WaterRect.Top := WaterRect.Bottom - Round((WaterRect.Bottom - WaterRect.Top) * FWaterLevel / 100);
  Canvas.Pen.Color := POOL_WATER_LINE_COLOR;
  Canvas.Brush.Color := POOL_WATER_COLOR;
  Canvas.Rectangle(WaterRect);

  // Линия целевого уровня
  TargetY := PoolRect.Bottom - Round((PoolRect.Bottom - PoolRect.Top) * TARGET_POOL_LEVEL / 100);
  Canvas.Pen.Style := psDash;
  Canvas.Pen.Color := TARGET_LEVEL_COLOR;
  Canvas.Pen.Width := 2;
  Canvas.Line(PoolRect.Left, TargetY, PoolRect.Right, TargetY);
  Canvas.Pen.Style := psSolid;

  // Толщина труб (1% от минимального размера)
  PipeThickness := Math.Max(2, Round(Min(Width, Height) * 0.01));

  // Труба наполнения (слева)
  PipeInY := PoolRect.Top + Round((PoolRect.Bottom - PoolRect.Top) * 0.2);
  Canvas.Pen.Color := clBlue;
  Canvas.Pen.Width := PipeThickness;
  Canvas.Line(
    PoolRect.Left - Round(Width * 0.04),
    PipeInY,
    PoolRect.Left,
    PipeInY
  );

  // Текст притока
  Canvas.Brush.Color := clNone;
  Canvas.Font.Size := Max(8, Round(Min(Width, Height) * 0.02));
  Canvas.TextOut(
    PoolRect.Left + Round(Width * 0.02),
    PipeInY - 20,
    Format('Приток: %.1f', [FInflowRate])
  );

  // Труба слива (справа)
  PipeOutY := PoolRect.Top + Round((PoolRect.Bottom - PoolRect.Top) * 0.6);
  Canvas.Pen.Color := clRed;
  Canvas.Pen.Width := PipeThickness;
  Canvas.Line(
    PoolRect.Right,
    PipeOutY,
    PoolRect.Right + Round(Width * 0.04),
    PipeOutY
  );

  // Текст cтока
  Canvas.TextOut(
    PoolRect.Right - Round(Width * 0.2),
    PipeOutY - 15,
    Format('Сток: %.1f', [FOutflowRate])
  );

  // Отображаем уровень воды (центрированный)
  Canvas.Font.Size := Math.Max(10, Round(Min(Width, Height) * 0.02));
  TextSize := Canvas.TextExtent(Format('Уровень: %.1f%%', [FWaterLevel]));
  Canvas.TextOut(
    (Width - TextSize.cx) div 2,
    PoolRect.Top - Round(Height * 0.055),
    Format('Уровень: %.1f%%', [FWaterLevel])
  );
end;

procedure TPoolVisualization.Resize;
begin
  inherited Resize;
  Invalidate; // Перерисовываем при изменении размера
end;

end.
