unit PIDSystem;

{$mode objfpc}{$H+}

interface

uses
  Classes, ControlTimer, PIDController, Definitions;

type
  TPIDSystem = class
  private
    FPIDController: TPIDController;
    FTimer: IControlTimer;
    FCurrentValue: Double;
    procedure ProcessControlCycle(Sender: TObject);
  public
    constructor Create(Timer: IControlTimer);
    destructor Destroy; override;
    procedure Start;
    procedure Stop;
    procedure Reset;
    property CurrentValue: Double read FCurrentValue;
  end;

implementation

constructor TPIDSystem.Create(Timer: IControlTimer);
begin
  FTimer := Timer;
  FTimer.Start;
  FTimer.OnTick := @ProcessControlCycle;
  FPIDController := TPIDController.Create(KP, KI, KD, TARGET_POOL_LEVEL);
  Reset;
end;

destructor TPIDSystem.Destroy;
begin
  FPIDController.Free;
  inherited Destroy;
end;

procedure TPIDSystem.Start;
begin
  FTimer.Start;
end;

procedure TPIDSystem.Stop;
begin
  FTimer.Stop;
end;

procedure TPIDSystem.Reset;
begin
  FCurrentValue := START_WATER_LEVEL;
end;

procedure TPIDSystem.ProcessControlCycle(Sender: TObject);
var
  OutputValue: Double;
begin
  OutputValue := FPIDController.Compute(FCurrentValue);

  // Эмуляция динамики системы
  FCurrentValue := FCurrentValue +
                   OutputValue * WATER_LEVEL_UPDATE_RATIO -
                   OUTFLOW_RATE * WATER_LEVEL_UPDATE_RATIO;

  // Ограничение значения
  if FCurrentValue < 0 then FCurrentValue := 0;
  if FCurrentValue > MAX_POOL_LEVEL then FCurrentValue := MAX_POOL_LEVEL;
end;

end.
