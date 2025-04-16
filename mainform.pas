unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ComCtrls, PIDController, PoolVisualization, WaterLevelGraph, Definitions,
  Math;

type
  { TMainForm }

  { TForm1 }

  TForm1 = class(TForm)
    btnReset: TButton;
    btnStart: TButton;
    chkPIDEnabled: TCheckBox;
    controlPanel: TPanel;
    lbInFlow: TLabel;
    lbOutFlow: TLabel;
    panelGraph: TPanel;
    panelPool: TPanel;
    StatusTimer: TTimer;
    Splitter1: TSplitter;
    tbInflowRate: TTrackBar;
    tbOutFlowRate: TTrackBar;
    procedure btnResetClick(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure StatusTimerTimer(Sender: TObject);
    procedure tbOutFlowRateChange(Sender: TObject);
  private
    FPool: TPoolVisualization;
    FGraph: TWaterLevelGraph;
    FPIDController: TPIDController;
    FCurrentValue: Double;
    FOutputValue: Double;
    procedure UpdateControls;
    procedure UpdateWaterSystem;
    procedure UpdateVisuals;
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
    // Инициализация интерфейса
    chkPIDEnabled.Caption := PID_CHECKBOX_LABEL;
    chkPIDEnabled.Checked := PID_ENABLED_DEFAULT;

    // Создаем визуализацию бассейна
    FPool := TPoolVisualization.Create(panelPool);
    FPool.Parent := panelPool;
    FPool.Align := alClient;
    FPool.WaterLevel := START_WATER_LEVEL;

    // Создаем график
    FGraph := TWaterLevelGraph.Create(panelGraph);
    FGraph.Parent := panelGraph;
    FGraph.Align := alClient;
    FGraph.TargetLevel := TARGET_POOL_LEVEL;

    // Создаем PID-контроллер
    FPIDController := TPIDController.Create(KP, KI, KD, TARGET_POOL_LEVEL);
    FCurrentValue := START_WATER_LEVEL;

    // Настраиваем таймер
    StatusTimer.Interval := UPDATE_TIMER_MS;
    StatusTimer.Enabled := False;

    btnReset.Caption := BUTTON_RESET_CAPTION;
    btnStart.Caption := BUTTON_START_CAPTION;

    lbInflow.Caption := INFLOW_PIPE;
    lbOutflow.Caption := OUTFLOW_PIPE;

end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
    FPIDController.Free;
    FPool.Free;
    FGraph.Free;
end;

procedure TForm1.FormResize(Sender: TObject);
var
    panelWidth, panelWidth095, panelWidth005 : integer;
begin
    panelWidth := Round(Width / 2);
    panelWidth095 := Round(panelWidth * 0.95);
    panelWidth005 := Round(panelWidth * 0.05);

    panelPool.Width:= panelWidth;
    panelPool.Left:=panelWidth005;
    panelGraph.Width:= panelWidth - panelWidth005;
    panelGraph.Left := panelWidth + panelWidth005;

    tbInflowRate.Left := panelWidth005;
    tbInflowRate.Width:= panelWidth095;
    tbOutFlowRate.Left:= panelWidth;
    tbOutFlowRate.Width:= panelWidth095;

    lbInflow.Left := panelWidth005;
    lbOutflow.Left := panelWidth;
end;

procedure TForm1.btnStartClick(Sender: TObject);
begin
  StatusTimer.Enabled := not StatusTimer.Enabled;
  if StatusTimer.Enabled then
    btnStart.Caption := BUTTON_STOP_CAPTION
  else
    btnStart.Caption := BUTTON_START_CAPTION;
end;

procedure TForm1.FormActivate(Sender: TObject);
begin
    Caption := WINDOW_TITLE;
    tbOutFlowRate.Position := OUTFLOW_RATE;
    tbInFlowRate.Position := OUTFLOW_RATE;
end;


procedure TForm1.btnResetClick(Sender: TObject);
begin
  if StatusTimer.Enabled then
      btnStartClick(Sender);

  chkPIDEnabled.Checked := false;
  FCurrentValue := POOL_DEFAULT_LEVEL;

  tbOutFlowRate.Position := OUTFLOW_RATE;
  tbInFlowRate.Position := OUTFLOW_RATE;
  FPool.InflowRate := OUTFLOW_RATE;
  FPool.OutflowRate := OUTFLOW_RATE;

  FGraph.Clear;
  UpdateVisuals;
  UpdateControls;
end;

procedure TForm1.StatusTimerTimer(Sender: TObject);
begin
    UpdateWaterSystem;
    UpdateControls;
    UpdateVisuals;
end;

procedure TForm1.tbOutFlowRateChange(Sender: TObject);
begin
    FPool.OutflowRate := tbOutFlowRate.Position;
end;

procedure TForm1.UpdateWaterSystem;
begin
    if chkPIDEnabled.Checked then
        FOutputValue := FPIDController.Compute(FCurrentValue);
end;

procedure TForm1.UpdateControls;
begin
    if chkPIDEnabled.Checked then
        tbInFlowRate.Position := Round(FOutputValue)
    else
        FPool.InflowRate := tbInFlowRate.Position;

    FPool.OutflowRate := tbOutFlowRate.Position;

end;

procedure TForm1.UpdateVisuals;
begin
    if chkPIDEnabled.Checked then
        FPool.InflowRate := FOutputValue;

    // Обновляем уровень воды
    FCurrentValue := FCurrentValue + (FPool.InflowRate - FPool.OutflowRate) * WATER_LEVEL_UPDATE_RATIO;
    FCurrentValue := Max(0, Min(MAX_POOL_LEVEL, FCurrentValue));

    // Обновляем визуализацию
    FPool.WaterLevel := FCurrentValue;
    FGraph.AddDataPoint(FCurrentValue);
end;

end.
